#include "docopt_fish.h"
#include "docopt_fish_types.h"
#include "docopt_fish_grammar.h"
#include <memory>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <set>

using std::vector;
using std::string;
using std::wstring;

UNUSED
static const wstring &widen(const wstring &t) {
    return t;
}

UNUSED
static wstring widen(const string &t) {
    wstring result;
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
    
    // How much of source remains to be parsed
    range_t remaining_range;
    
    // The initial indent for the usage_t that we are parsing. If we reach a line that is indented further than this, we treat it as a continuation of the previous line.
    size_t initial_indent;
    
    // Errors we generate
    vector<error_t<string_t> > errors;
    
    parse_context_t(const string_t &src, const range_t &usage_range, const option_list_t &shortcuts) : source(&src), shortcut_options(&shortcuts), remaining_range(usage_range), initial_indent(-1)
    {}
    
    /* Consume leading whitespace. Newlines are meaningful if their associated lines are indented the same or less than initial_indent. If they are indented more, we swallow those. */
    void consume_leading_whitespace() {
        for (;;) {
            scan_while(*source, &remaining_range, char_is_space_or_tab);
            const range_t saved_remaining = this->remaining_range;
            if (scan_1_char(*source, &remaining_range, '\n').empty()) {
                // Not a newline
                remaining_range = saved_remaining;
                break;
            }
            
            // Got a newline. Compare our indent. If we're indented more, we swallow it and keep going!
            size_t indent = indent_for_current_line();
            if (indent <= initial_indent) {
                this->remaining_range = saved_remaining;
                break;
            }
        }
    }
    
    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return remaining_range.length == 0;
    }
    
    /* Rounds a value up to the next tabstop */
    static size_t round_to_next_tabstop(size_t val) {
        const size_t tabstop = 4;
        return (val + tabstop) / tabstop * tabstop;
    }

    /* Returns how much the current line is indented. The tabstop is 4. If the line is empty, we return -1 (which is huge, since we're unsigned).
       This needs to look backwards in the string, even outside our range. Consider:
       
           Usage: prog foo
                  prog bar
       
       The indent of "prog foo" must be 7.
    */
    size_t indent_for_current_line() const {
        size_t result = 0;
        
        // Walk backwards until we hit the string beginning, or a newline, then sum until we hit the range start
        // We can't sum backwards because we can't compute the tabstops
        size_t start = remaining_range.start;
        while (start > 0 && source->at(start - 1) != '\n')
        {
            start -= 1;
        }
        for (size_t idx = start; idx < remaining_range.start; idx++)
        {
            char_t c = source->at(idx);
            if (c == '\t') {
                result = round_to_next_tabstop(result);
            } else {
                result += 1;
            }
        }
        
        // Walk forwards until we hit the next newline
        for (size_t idx = remaining_range.start; idx < remaining_range.end(); idx++) {
            char_t c = source->at(idx);
            if (c == '\t') {
                result = round_to_next_tabstop(result);
            } else if (c == ' ') {
                result += 1;
            } else if (c == '\n') {
                // Empty line
                result = -1;
                break;
            } else {
                // Not a whitespace character.
                break;
            }
        }
        return result;
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
    
    bool peek(const char *s) {
        const range_t saved_range = remaining_range;
        token_t tok;
        bool result = scan(s, &tok);
        remaining_range = saved_range;
        return result;
    }
    
    token_t peek_word() {
        const range_t saved_range = remaining_range;
        token_t tok;
        scan_word(&tok);
        remaining_range = saved_range;
        return tok;
    }

    /* Helpers to parse when the productions are fixed. */
    
    template<typename PARENT, typename REQ_CHILD, typename OPT_CHILD>
    bool parse_required_then_optional(auto_ptr<PARENT> *result) {
        auto_ptr<REQ_CHILD> child1;
        if (! parse(&child1)) {
            return false;
        }
        if (child1.get() == NULL) {
            return true;
        }
        
        auto_ptr<OPT_CHILD> child2;
        if (! parse(&child2)) {
            return false;
        }
        result->reset(new PARENT(child1, child2));
        return true;
    }

    template<typename PARENT, typename CHILD>
    bool parse_1_disallowing_null(auto_ptr<PARENT> *result) {
        bool success = false;
        auto_ptr<CHILD> child;
        if (parse(&child)) {
            // This variant requires that successful parses return non-null
            assert(child.get() != NULL);
            result->reset(new PARENT(child));
            success = true;
        }
        return result;
    }
    
    // Like parse1, except if the child comes back NULL we provide NULL too
    template<typename PARENT, typename CHILD>
    bool parse_1_or_empty(auto_ptr<PARENT> *result) {
        auto_ptr<CHILD> child;
        if (! parse(&child)) {
            return false;
        }
        if (child.get()) {
            result->reset(new PARENT(child));
        }
        return true;
    }
    
    #pragma mark Parse functions
    
    bool parse(auto_ptr<alternation_list_t> *result) {
        return parse_required_then_optional<alternation_list_t, expression_list_t, or_continuation_t>(result);
    }
    
    bool parse(auto_ptr<expression_list_t> *result)
    {
        return parse_required_then_optional<expression_list_t, expression_t, opt_expression_list_t>(result);
    }
    
    bool parse(auto_ptr<opt_expression_list_t> *result)
    {
        // Only return if we are able to parse a non-null expression_list
        auto_ptr<expression_list_t> child;
        if (! parse(&child)) {
            return false;
        }
        if (child.get()) {
            result->reset(new opt_expression_list_t(child));
        }
        return true;
    }
    
    bool parse(auto_ptr<usage_t> *result, bool first)
    {
        // Suck up empty lines.
        while (scan('\n')) {
            continue;
        }
        
        /* Determine the initial indent before consuming whitespace. Lines that are indented less than this are considered part of the existing usage */
        this->initial_indent = this->indent_for_current_line();
        
        consume_leading_whitespace();
        if (this->is_at_end() && ! first) {
            return true;
        }
        
        bool success = false;
        token_t word;
        if (! this->scan_word(&word)) {
            append_error(&this->errors, this->remaining_range.start, error_missing_program_name, "Missing program name");
        } else {
            auto_ptr<alternation_list_t> el;
            bool parsed_el = parse(&el);
            // Consume as many newlines as we can, and then try to generate the tail
            while (scan('\n')) {
                continue;
            }
            auto_ptr<usage_t> next;
            if (parse(&next, false /* not initial */)) {
                if (parsed_el) {
                    result->reset(new usage_t(word, el, next));
                } else {
                    result->reset(new usage_t(word, next));
                }
                success = true;
            }
        }
        return success;
    }

    bool parse(auto_ptr<or_continuation_t> *result) {
        token_t bar;
        if (this->scan('|', &bar)) {
            auto_ptr<alternation_list_t> al;
            if (parse(&al) && al.get()) {
                result->reset(new or_continuation_t(bar, al));
            } else {
                append_error(&this->errors, bar.range.start, error_trailing_vertical_bar, "Extra vertical bar");
                return false;
            }
        }
        // result may be left as null if there's no bar
        return true;
    }

    bool parse(auto_ptr<opt_ellipsis_t> *result) {
        token_t ellipsis;
        if (this->scan("...", &ellipsis)) {
            result->reset(new opt_ellipsis_t(ellipsis));
        }
        return true; // can't fail
    }
    
    bool parse(auto_ptr<options_shortcut_t> *result) {
        result->reset(new options_shortcut_t());
        return true; // can't fail
    }
    
    bool parse(auto_ptr<simple_clause_t> *result) {
        token_t word = this->peek_word();
        if (word.range.empty()) {
            // Nothing remaining to parse
            return true;
        }
        
        char_t c = this->source->at(word.range.start);
        if (c == '<') {
            return parse_1_disallowing_null<simple_clause_t, variable_clause_t>(result);
        } else if (c == '-' && word.range.length > 1) {
            // A naked '-', is to be treated as a fixed value
            return parse_1_disallowing_null<simple_clause_t, option_clause_t>(result);
        } else {
            return parse_1_disallowing_null<simple_clause_t, fixed_clause_t>(result);
        }
    }
    
    bool parse(auto_ptr<option_clause_t> *result) {
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
            // This second test is to avoid matching '--'
            if (range.length > 1 && src.at(range.start) == '-' && ! (range.length == 2 && src.at(range.start + 1) == '-')) {
                // It's an option
                option_t opt_from_usage_section = option_t::parse_from_string(src, &range, &this->errors);
                
                // See if we have a corresponding option in the options section
                const option_t *opt_from_options_section = NULL;
                for (size_t i=0; i < this->shortcut_options->size(); i++) {
                    const option_t &test_op = this->shortcut_options->at(i);
                    if (opt_from_usage_section.has_same_name(test_op, src)) {
                        opt_from_options_section = &test_op;
                        break;
                    }
                }
                
                // We may have to parse a variable
                if (opt_from_usage_section.name.length > 0 && opt_from_usage_section.separator == option_t::sep_space) {
                    // Looks like an option without a separator. See if the next token is a variable
                    range_t next = this->peek_word().range;
                    if (next.length > 2 && src.at(next.start) == '<' && src.at(next.end() - 1) == '>') {
                        // It's a variable. See if we have a presence in options.
                        bool options_section_implies_no_value = (opt_from_options_section && opt_from_options_section->value.empty());
                        if (! options_section_implies_no_value) {
                            token_t variable;
                            bool scanned = this->scan_word(&variable);
                            assert(scanned); // Should always succeed, since we peeked at the word
                            word.range.merge(variable.range);
                            opt_from_usage_section.value = variable.range;
                        }
                    }
                }
                
                // Use the option from the Options section in preference to the one from the Usage section, since the options one has more information like the corresponding long name and description
                result->reset(new option_clause_t(word, opt_from_options_section ? *opt_from_options_section : opt_from_usage_section));
            }
        }
        return true;
    }
    
    bool parse(auto_ptr<fixed_clause_t> *result) {
        token_t word;
        if (this->scan_word(&word)) {
            // TODO: handle invalid commands like foo<bar>
            result->reset(new fixed_clause_t(word));
        }
        return true;
    }
    
    bool parse(auto_ptr<variable_clause_t> *result) {
        token_t word;
        if (this->scan_word(&word)) {
            // TODO: handle invalid variables like foo<bar>
            result->reset(new variable_clause_t(word));
        }
        return true;
    }

    
    bool parse(auto_ptr<expression_t> *result) {
        if (this->is_at_end()) {
            return true;
        }
        
        bool success = true;
        token_t token;
        // Note that options must come before trying to parse it as a list, because "[options]" itself looks like a list
        if (this->scan("[options]", &token)) {
            auto_ptr<options_shortcut_t> shortcut;
            if (! parse(&shortcut)) {
                success = false;
            } else {
                result->reset(new expression_t(shortcut));
            }
        } else if (this->scan('(', &token) || this->scan('[', &token)) {
            auto_ptr<alternation_list_t> contents;
            if (! parse(&contents)) {
                success = false;
            } else {
                char_t c = this->source->at(token.range.start);
                assert(c == char_t('(') || c == char_t('['));
                bool is_paren = (c == char_t('('));
                token_t close_token;
                if (this->scan(char_t(is_paren ? ')' : ']'), &close_token)) {
                    auto_ptr<opt_ellipsis_t> ellipsis;
                    bool p = parse(&ellipsis);
                    assert(p); // should never fail
                    result->reset(new expression_t(token, is_paren, contents, close_token, ellipsis));
                } else {
                    // No closing bracket or paren
                    if (is_paren) {
                        append_error(&this->errors, token.range.start, error_missing_close_paren, "Missing ')' to match opening '('");
                    } else {
                        append_error(&this->errors, token.range.start, error_missing_close_bracket, "Missing ']' to match opening '['");
                    }
                    success = false;
                }
            }
        } else if (this->scan("...", &token)) {
            // Indicates leading ellipsis
            // TODO: generate error
            success = false;
        } else if (this->peek("|")) {
            // End of an alternation list
        } else {
            success = parse_required_then_optional<expression_t, simple_clause_t, opt_ellipsis_t>(result);
        }
        
        return success;
    }
};

template<typename string_t>
usage_t *parse_usage(const string_t &src, const range_t &src_range, const option_list_t &shortcut_options, vector<error_t<string_t> > *out_errors)
{
    parse_context_t<string_t> ctx(src, src_range, shortcut_options);
    auto_ptr<usage_t> result;
    bool parsed = ctx.template parse(&result, true /* initial */);
    
    // If we get NULL back, it should return false
    assert(! (result.get() == NULL && parsed));
    // If we get NULL back, we should always have an error
    assert(! (result.get() == NULL && ctx.errors.empty()));
    
    if (out_errors) {
        out_errors->insert(out_errors->end(), ctx.errors.begin(), ctx.errors.end());
    }
    
    usage_t *usage = result.release();
    return usage;
}

// Force template instantiation
template usage_t *parse_usage<string>(const string &, const range_t &, const option_list_t &, vector<error_t<string> > *);
template usage_t *parse_usage<wstring>(const wstring &, const range_t &, const option_list_t &shortcut_options, vector<error_t<wstring> > *);


CLOSE_DOCOPT_IMPL /* namespace */

