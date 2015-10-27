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
#include <vector>

enum parse_result_t {
    parsed_ok,
    parsed_error,
    parsed_done
};

namespace docopt_fish
OPEN_DOCOPT_IMPL

#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

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

    inline range_t scan_1_char(range_t *remaining, char_t c) {
        range_t result(remaining->start, 0);
        if (result.end() < remaining->end() && this->source->at(result.end()) == c) {
            result.length += 1;
            remaining->start += 1;
            remaining->length -= 1;
        }
        return result;
    }

    template<typename T>
    inline range_t scan_while(range_t *remaining, T func) {
        size_t idx = remaining->start;
        const size_t end = remaining->end();
        assert(end <= this->source->size());
        while (idx < end && func((*this->source)[idx])) {
            idx++;
        }
        range_t result(remaining->start, idx - remaining->start);
        remaining->start = result.end();
        remaining->length -= result.length;
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
            scan_while(&remaining_range, char_is_space_or_tab);
            const range_t saved_remaining = this->remaining_range;
            if (scan_1_char(&remaining_range, '\n').empty()) {
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
        storage.range = scan_1_char(&remaining_range, c);
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
            result.merge(scan_while(&remaining_range, char_is_valid_in_word));
            // Scan bracketed sequence
            range_t bracket_start = scan_1_char(&remaining_range, '<');
            if (! bracket_start.empty()) {
                scan_while(&remaining_range, char_is_valid_in_bracketed_word);
                range_t bracket_end = scan_1_char(&remaining_range, '>');
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
    
    // Given a vector of T, try parsing and then appending a T
    template<typename T>
    inline parse_result_t try_parse_appending(vector<T> *vec) {
        vec->resize(vec->size() + 1);
        parse_result_t status = this->parse(&vec->back());
        if (status != parsed_ok) {
            vec->pop_back();
        }
        return status;
    }
    
    // Given a deep_ptr, try parsing it
    template<typename T>
    inline parse_result_t try_parse_auto(deep_ptr<T> *p) {
        p->reset(new T());
        return this->parse(p->get());
    }
    
    #pragma mark Parse functions
    
    parse_result_t parse(alternation_list_t *result) {
        parse_result_t status = parsed_ok;
        bool first = true;
        // We expect only one alternation, but need to reserve one more for try_parse_appending
        result->alternations.reserve(2);
        while (status == parsed_ok) {
            // Scan a vert bar if we're not first
            token_t bar;
            if (! first && ! this->scan('|', &bar)) {
                status = parsed_done;
                break;
            }
            status = try_parse_appending(&result->alternations);
            if (status == parsed_done) {
                if (! first) {
                    append_error(&this->errors, bar.range.start, error_trailing_vertical_bar, "Trailing vertical bar");
                    status = parsed_error;
                }
                break;
            }
            first = false;
        }
        if (status == parsed_done) {
            /* We may get an empty alternation list if we are just the program name. In that case, ensure we have at least one. */
            if (result->alternations.empty()) {
                result->alternations.resize(1);
            }
            
            /* Hackish place to do this */
            collapse_corresponding_options(result);
            
            status = parsed_ok;
        }
        return status;
    }
    
    parse_result_t parse(expression_list_t *result) {
        result->expressions.reserve(6);
        parse_result_t status = parsed_ok;
        size_t count = -1;
        while (status == parsed_ok) {
            status = try_parse_appending(&result->expressions);
            count++;
        }
        // Return OK if we got at least one thing
        if (status == parsed_done && count > 0) {
            status = parsed_ok;
        }
        return status;
    }
    
    // Parse a usage_t
    parse_result_t parse(usage_t *result) {
        // Suck up empty lines.
        while (scan('\n')) {
            continue;
        }
        
        /* Determine the initial indent before consuming whitespace. Lines that are indented more than this are considered part of the existing usage */
        this->initial_indent = this->indent_for_current_line();
        
        consume_leading_whitespace();
        if (this->is_at_end()) {
            return parsed_done;
        }
        
        bool scanned = this->scan_word(&result->prog_name);
        assert(scanned); // else we should not have tried to parse this as a usage
        return parse(&result->alternation_list);
    }
    
    // Parse ellipsis
    parse_result_t parse(opt_ellipsis_t *result) {
        result->present = this->scan("...", &result->ellipsis);
        return parsed_ok; // can't fail
    }
    
    // "Parse" [options]
    parse_result_t parse(options_shortcut_t *result) {
        result->present = true;
        return parsed_ok; // can't fail
    }
    
    // Parse a simple clause
    parse_result_t parse(simple_clause_t *result) {
        token_t word = this->peek_word();
        if (word.range.empty()) {
            // Nothing remaining to parse
            return parsed_done;
        }
        
        char_t c = this->source->at(word.range.start);
        if (c == '<') {
            return this->try_parse_auto(&result->variable);
        } else if (c == '-' && word.range.length > 1) {
            // A naked '-', is to be treated as a fixed value
            return this->try_parse_auto(&result->option);
        } else {
            return this->try_parse_auto(&result->fixed);
        }
    }

    // Parse options clause
    parse_result_t parse(option_clause_t *result) {
        token_t word;
        if (! this->scan_word(&word)) {
            return parsed_done;
        }
        
        // TODO: handle --
        
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
            option_t opt_from_usage_section;
            if (! option_t::parse_from_string(src, &range, &opt_from_usage_section, &this->errors)) {
                return parsed_error;
            }
            assert(opt_from_usage_section.best_name().length > 0);
            
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
            if (opt_from_usage_section.separator == option_t::sep_space) {
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
            result->word = word;
            result->option = opt_from_options_section ? *opt_from_options_section : opt_from_usage_section;
        }
        return parsed_ok;
    }
    
    // Parse a fixed argument
    parse_result_t parse(fixed_clause_t *result) {
        if (this->scan_word(&result->word)) {
            // TODO: handle invalid commands like foo<bar>
            return parsed_ok;
        } else {
            return parsed_done;
        }
    }
    
    // Parse a variable argument
    parse_result_t parse(variable_clause_t *result) {
        if (this->scan_word(&result->word)) {
            // TODO: handle invalid variables like foo<bar>
            return parsed_ok;
        } else {
            return parsed_done;
        }
    }

    // Parse a general expression
    parse_result_t parse(expression_t *result) {
        if (this->is_at_end()) {
            return parsed_done;
        }
        
        parse_result_t status; // should be set in every branch
        token_t token;
        // Note that options must come before trying to parse it as a list, because "[options]" itself looks like a list
        if (this->scan("[options]", &token)) {
            result->production = 3;
            status = this->parse(&result->options_shortcut);
        } else if (this->scan('(', &token) || this->scan('[', &token)) {
            status = this->try_parse_auto(&result->alternation_list);
            if (status != parsed_error) {
                char_t c = this->source->at(token.range.start);
                assert(c == char_t('(') || c == char_t('['));
                bool is_paren = (c == char_t('('));
                token_t close_token;
                if (this->scan(char_t(is_paren ? ')' : ']'), &close_token)) {
                    result->production = is_paren ? 1 : 2;
                    parse(&result->opt_ellipsis); // never fails
                } else {
                    // No closing bracket or paren
                    if (is_paren) {
                        append_error(&this->errors, token.range.start, error_missing_close_paren, "Missing ')' to match opening '('");
                    } else {
                        append_error(&this->errors, token.range.start, error_missing_close_bracket, "Missing ']' to match opening '['");
                    }
                    status = parsed_error;
                }
            }
        } else if (this->scan("...", &token)) {
            append_error(&this->errors, token.range.start, error_leading_ellipsis, "Ellipsis may only follow an expression");
            status = parsed_error;
        } else if (this->peek("|")) {
            // End of an alternation list
            status = parsed_done;
        } else {
            // Simple clause
            status = try_parse_auto(&result->simple_clause);
            if (status != parsed_error) {
                result->production = 0;
                parse(&result->opt_ellipsis); // never fails
            }
        }
        return status;
    }

    /* Given an expression list, if it wraps a single option, return a pointer to that option.
     Else return NULL. */
    static option_t *single_option(expression_list_t *list) {
        if (list->expressions.size() != 1) {
            return NULL;
        }
        expression_t *expr = &list->expressions[0];
        simple_clause_t *simple_clause = expr->simple_clause.get();
        option_clause_t *option_clause = simple_clause ? simple_clause->option.get() : NULL;
        return option_clause ? &option_clause->option : NULL;
    }
    
    
    /* For example:
     usage: prog [-e | --erase]
     prog [-a <name> | --add <name>]
     
     Here we need to mark -e's corresponding long name as --erase, and same for -a/--add.
     This applies if we have exactly two options.
     */
    void collapse_corresponding_options(alternation_list_t *list) {
        assert(list != NULL);
        // Must have exactly 2 alternations
        if (list->alternations.size() != 2) {
            return;
        }
        option_t *first = single_option(&list->alternations[0]);
        option_t *second = single_option(&list->alternations[1]);

        /* Both options must be non-NULL, and they must not have overlapping name types, and their values must agree (perhaps both empty) */
        bool options_correspond = (first != NULL && second != NULL &&
                                   ! first->name_types_overlap(*second) &&
                                   0 == this->source->compare(first->value.start, first->value.length, *this->source,
                                                              second->value.start, second->value.length));
        if (options_correspond) {
            // Merge them. Note: this merge_from call writes deep into our tree!
            // Then delete the second alternation
            first->merge_from(*second);
            list->alternations.pop_back();
            assert(list->alternations.size() == 1);
        }
    }
};

void usage_t::make_default() {
    // hackish?
    const std::string src = "command [options]";
    vector<error_t<std::string> > *null_errors = NULL;
    parse_one_usage(src, range_t(0, src.size()), option_list_t(), this, null_errors);
}

template<typename string_t>
bool parse_one_usage(const string_t &src, const range_t &src_range, const option_list_t &shortcut_options, usage_t *out_usage, vector<error_t<string_t> > *out_errors)
{
    parse_context_t<string_t> ctx(src, src_range, shortcut_options);
    parse_result_t status = ctx.template parse(out_usage);
    assert(! (status == parsed_error && ctx.errors.empty()));
    if (out_errors) {
        out_errors->insert(out_errors->end(), ctx.errors.begin(), ctx.errors.end());
    }
    return status != parsed_error;
}

// Force template instantiation
template bool parse_one_usage<std::string>(const std::string &, const range_t &, const option_list_t &, usage_t *out_usage, vector<error_t<std::string> > *);
template bool parse_one_usage<std::wstring>(const std::wstring &, const range_t &, const option_list_t &shortcut_options, usage_t *out_usage, vector<error_t<std::wstring> > *);


CLOSE_DOCOPT_IMPL /* namespace */

