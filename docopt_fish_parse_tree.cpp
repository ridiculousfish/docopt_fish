#include "docopt_fish.h"
#include "docopt_fish_types.h"
#include "docopt_fish_grammar.h"
#include <memory>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <iostream>
#include <numeric>
#include <set>

UNUSED
static const std::wstring &widen(const std::wstring &t) {
    return t;
}

UNUSED
static std::wstring widen(const std::string &t) {
    std::wstring result;
    result.insert(result.begin(), t.begin(), t.end());
    return result;
}

namespace docopt_fish
OPEN_DOCOPT_IMPL

#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

struct alternation_list;
struct expression_list_t;
struct opt_expression_list_t;
struct expression_t;
struct or_continuation_t;
struct simple_clause_t;
struct opt_ellipsis_t;

/* Context passed around in our recursive descent parser */
template<typename string_t>
struct parse_context_t {
    typedef typename string_t::value_type char_t;

    static bool char_is_valid_in_word(char_t c) {
        const char *invalid = ".|()[],<> \t\n";
        const char *end = invalid + strlen(invalid);
        return std::find(invalid, end, c) == end;
    }

    static bool char_is_valid_in_bracketed_word(char_t c) {
        const char *invalid = "|()[]>\t\n";
        const char *end = invalid + strlen(invalid);
        return std::find(invalid, end, c) == end;
    }

    static bool char_is_space_or_tab(char_t c) {
        return c == ' ' || c == '\t';
    }

    static range_t scan_1_char(const string_t &str, range_t *remaining, char_t c) {
        range_t result(remaining->start, 0);
        if (result.end() < remaining->end() && str.at(result.end()) == c) {
            result.length += 1;
            remaining->start += 1;
            remaining->length -= 1;
        }
        return result;
    }

    template<typename T>
    static range_t scan_while(const string_t &str, range_t *remaining, T func) {
        range_t result(remaining->start, 0);
        while (result.end() < remaining->end() && func(str.at(result.end()))) {
            result.length += 1;
            remaining->start += 1;
            remaining->length -= 1;
        }
        return result;
    }

    
     // Note unowned pointer reference. A parse context is stack allocated and transient.
    const string_t *source;
    const option_list_t *shortcut_options;
    range_t remaining_range;
    
    parse_context_t(const string_t &src, const range_t &usage_range, const option_list_t &shortcuts) : source(&src), shortcut_options(&shortcuts), remaining_range(usage_range)
    {}
    
    /* Consume leading whitespace, except newlines, which are meaningful and therefore tokens in their own right */
    void consume_leading_whitespace() {
        scan_while(*source, &remaining_range, char_is_space_or_tab);
    }
    
    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return remaining_range.length == 0;
    }
    
    bool scan(char_t c, token_t *tok = NULL) {
        this->consume_leading_whitespace();
        token_t storage;
        storage.range = scan_1_char(*source, &remaining_range, c);
        if (tok) {
            *tok = storage;
        }
        return ! storage.range.empty();
    }
    
    bool scan(const char *c, token_t *tok) {
        this->consume_leading_whitespace();
        bool success = true;
        token_t string_tok;
        const range_t saved_remaining = this->remaining_range;
        for (size_t i=0; c[i] != '\0'; i++) {
            token_t char_tok;
            if (! this->scan(c[i], &char_tok)) {
                success = false;
                break;
            }
            string_tok.range.merge(char_tok.range);
        }
        
        // If we did not scan all the characters, restore our initial state
        if (! success) {
            this->remaining_range = saved_remaining;
            string_tok.range = range_t(0, 0);
        }
        if (tok) {
            *tok = string_tok;
        }
        return success;
    }
    
    bool scan_word(token_t *tok) {
        this->consume_leading_whitespace();
        /* A word may have embedded <...>. These may contain spaces. They may also abut: --foo<abc def>bar' is one word. So we use a loop. */
        range_t result;
        for (;;) {
            // Scan non-bracketed sequence. This may be empty (in which case the merge does nothing)
            result.merge(scan_while(*source, &remaining_range, char_is_valid_in_word));
            // Scan bracketed sequence
            range_t bracket_start = scan_1_char(*source, &remaining_range, '<');
            if (! bracket_start.empty()) {
                scan_while(*source, &remaining_range, char_is_valid_in_bracketed_word);
                range_t bracket_end = scan_1_char(*source, &remaining_range, '>');
                if (bracket_end.empty()) {
                    // TODO: report unclosed bracket
                } else {
                    // Merging the bracket ranges will include the contents
                    result.merge(bracket_start);
                    result.merge(bracket_end);
                }
            }
            // If we got an empty bracket range, then we're done; otherwise start again
            if (bracket_start.empty()) {
                break;
            }
        }
        tok->range = result;
        return ! tok->range.empty();
    }
    
    token_t peek_word() {
        const range_t saved_range = remaining_range;
        token_t tok;
        scan_word(&tok);
        remaining_range = saved_range;
        return tok;
    }
    
    /* Helpers to parse when the productions are fixed. */
    template<typename PARENT, typename CHILD1, typename CHILD2>
    PARENT *parse_2() {
        PARENT *result = NULL;
        auto_ptr<CHILD1> child1(parse<CHILD1>());
        if (child1.get()) {
            auto_ptr<CHILD2> child2(parse<CHILD2>());
            if (child2.get()) {
                result = new PARENT(child1, child2);
            }
        }
        return result;
    }

    template<typename PARENT, typename CHILD>
    PARENT *parse_1_or_empty() {
        PARENT *result = NULL;
        auto_ptr<CHILD> child(parse<CHILD>());
        if (child.get()) {
            result = new PARENT(child);
        } else {
            result = new PARENT();
        }
        return result;
    }
    
    #pragma mark Parse functions
    
    template<typename T>
    T *parse()
    {
        T* dummy = NULL;
        return this->parse(dummy);
    }
    
    alternation_list_t *parse(alternation_list_t *dummy)
    {
        return parse_2<alternation_list_t, expression_list_t, or_continuation_t>();
    }
    
    expression_list_t *parse(expression_list_t *dummy)
    {
        return parse_2<expression_list_t, expression_t, opt_expression_list_t>();
    }
    
    opt_expression_list_t *parse(opt_expression_list_t *dummy)
    {
        return parse_1_or_empty<opt_expression_list_t, expression_list_t>();
    }
    
    usage_t *parse(usage_t *dummy)
    {
        // Suck up leading newlines
        while (this->scan('\n')) {
            ;
        }
        
        // If we reach the end, we return an empty usage
        if (this->is_at_end()) {
            return new usage_t();
        }
        
        usage_t *result = NULL;
        
        // TODO: generate error for missing word name
        token_t word;
        if (this->scan_word(&word)) {
            auto_ptr<alternation_list_t> el(parse<alternation_list_t>());
            // Consume as many newlines as we can, and then try to generate the tail
            while (this->scan('\n')) {
                continue;
            }
            auto_ptr<usage_t> next(parse<usage_t>());
            
            if (el.get()) {
                result = new usage_t(word, el, next);
            } else {
                result = new usage_t(word, next);
            }

        }
        return result;
    }

    or_continuation_t *parse(or_continuation_t *dummy) {
        or_continuation_t *result = NULL;
        token_t bar;
        if (this->scan('|', &bar)) {
            auto_ptr<alternation_list_t> al(parse<alternation_list_t>());
            if (al.get()) {
                result = new or_continuation_t(bar, al);
            } else {
                // TODO: generate an error about trailing bar
                fprintf(stderr, "Trailing bar\n");
            }
        } else {
            result = new or_continuation_t();
        }
        return result;
    }

    opt_ellipsis_t *parse(opt_ellipsis_t *dummy) {
        token_t ellipsis;
        this->scan("...", &ellipsis);
        return new opt_ellipsis_t(ellipsis);
    }
    
    options_shortcut_t *parse(options_shortcut_t *dummy) {
        return new options_shortcut_t();
    }
    
    simple_clause_t *parse(simple_clause_t *dummy) {
        simple_clause_t *result = NULL;
        token_t word;
        if (this->scan_word(&word)) {
            /* Hack to support specifying parameter names inline.
             
             Consider usage like this:
               "usage: prog [-m <msg>]"
             
            There's two ways we can interpret this:
             1. An optional -m flag, followed by a positional parameter
             2. An -m option whose value is <msg>
            
            The Python reference docopt resolves this ambiguity by looking at the Options: section. If it finds a declaration of -m with a variable, then <msg> is assumed to be the value (interpretation #2); otherwise it's a positional (interpretation #1).
             
            But requiring a required positional after a flag seems very unlikely, so in this version we always use interpretation #2. We do this by treating it as one token '-m <msg>'. Note this token contains a space.
             
             One exception is if a delimeter is found, e.g.:
             
             "usage: prog [-m=<foo> <msg>]"
             
             Now <foo> is the value of the option m, and <msg> is positional.
             
             The second exception is if the option is also found in the Options: section without a variable:
             
                usage: prog -m <msg>
                options: -m
            
            Then we ignore the option.
             
            Also, if you really want interpretation #1, you can use parens:
             
               "usage: prog [-m (<msg>)]"
            */
            const string_t &src = *this->source;
            range_t range = word.range;
            // TODO: don't match '--'
            if (range.length > 1 && src.at(range.start) == '-') {
                // It's an option
                option_t opt = option_t::parse_from_string(src, &range);
                if (opt.name.length > 0 && opt.separator == option_t::sep_space) {
                    // Looks like an option without a separator. See if the next token is a variable
                    range_t next = this->peek_word().range;
                    if (next.length > 2 && src.at(next.start) == '<' && src.at(next.end() - 1) == '>') {
                        // It's a variable. See if we have a presence in options.
                        const option_t *opt_from_options_section = NULL;
                        for (size_t i=0; i < this->shortcut_options->size(); i++) {
                            const option_t &test_op = this->shortcut_options->at(i);
                            if (opt.has_same_name(test_op, src)) {
                                opt_from_options_section = &test_op;
                                break;
                            }
                        }
                        bool options_section_implies_no_value = (opt_from_options_section && opt_from_options_section->value.empty());
                        
                        if (! options_section_implies_no_value) {
                            token_t variable;
                            bool scanned = this->scan_word(&variable);
                            assert(scanned); // Should always succeed, since we peeked at the word
                            word.range.merge(variable.range);
                        }
                    }
                }
            }
            
            result = new simple_clause_t(word);
        }
        return result;
    }
    
    expression_t *parse(expression_t *dummy) {
        expression_t *result = NULL;
        if (this->is_at_end()) {
            return NULL;
        }
        
        token_t token;
        // Note that options must come before trying to parse it as a list, because "[options]" itself looks like a list
        if (this->scan("[options]", &token)) {
            auto_ptr<options_shortcut_t> shortcut(parse<options_shortcut_t>());
            if (shortcut.get()) {
                result = new expression_t(shortcut);
            }
        } else if (this->scan('(', &token) || this->scan('[', &token)) {
            auto_ptr<alternation_list_t> contents(parse<alternation_list_t>());
            if (contents.get()) {
                char_t c = this->source->at(token.range.start);
                assert(c == char_t('(') || c == char_t('['));
                bool is_paren = (c == char_t('('));
                token_t close_token;
                if (this->scan(char_t(is_paren ? ')' : ']'), &close_token)) {
                    auto_ptr<opt_ellipsis_t> ellipsis(parse<opt_ellipsis_t>());
                    assert(ellipsis.get() != NULL); // should never fail
                    result = new expression_t(token, is_paren, contents, close_token, ellipsis);
                } else {
                    // TODO: generate error of unclosed paren
                }
            }
        } else if (this->scan("...", &token)) {
            // Indicates leading ellipsis
            // TODO: generate error
        } else {
            auto_ptr<simple_clause_t> simple_clause(parse<simple_clause_t>());
            if (simple_clause.get()) {
                auto_ptr<opt_ellipsis_t> ellipsis(parse<opt_ellipsis_t>());
                assert(ellipsis.get() != NULL); // should never fail
                result = new expression_t(simple_clause, ellipsis);
            }
        }
        
        return result;
    }
};

template<typename string_t>
usage_t *parse_usage(const string_t &src, const range_t &src_range, const option_list_t &shortcut_options)
{
    parse_context_t<string_t> ctx(src, src_range, shortcut_options);
    return ctx.template parse<usage_t>();
}

// Force template instantiation
template usage_t *parse_usage<std::string>(const std::string &src, const range_t &src_range, const option_list_t &shortcut_options);
template usage_t *parse_usage<std::wstring>(const std::wstring &src, const range_t &src_range, const option_list_t &shortcut_options);


CLOSE_DOCOPT_IMPL /* namespace */

