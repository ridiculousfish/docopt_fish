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
struct parse_context_t {
    static bool char_is_valid_in_word(rstring_t::char_t c) {
        const char *invalid = ".|()[],<> \t\n";
        const char *end = invalid + strlen(invalid);
        return std::find(invalid, end, c) == end;
    }

    static bool char_is_valid_in_bracketed_word(rstring_t::char_t c) {
        const char *invalid = "|()[]>\t\n";
        const char *end = invalid + strlen(invalid);
        return std::find(invalid, end, c) == end;
    }
    
    // Note unowned pointer references in rstring_t. A parse context is stack allocated and transient.
    
    // Range of source remains to be parsed
    rstring_t remaining;

    const option_list_t *shortcut_options;
    
    // Errors we generate
    vector<error_t> errors;
    
    parse_context_t(const rstring_t &usage, const option_list_t &shortcuts) : remaining(usage), shortcut_options(&shortcuts) { }
    
    /* Consume leading whitespace. Newlines are meaningful if their associated lines are indented the same or less than initial_indent. If they are indented more, we swallow those. */
    void consume_leading_whitespace() {
        this->remaining.scan_while<rstring_t::char_is_whitespace>();
    }
    
    /* Returns true if there are no more next tokens */
    bool is_at_end() const {
        return this->remaining.empty();
    }
    
    // Try scanning a single character
    bool scan(char c, rstring_t *tok = NULL) {
        this->consume_leading_whitespace();
        rstring_t scanned = this->remaining.scan_1_char(c);
        if (tok) {
            *tok = scanned;
        }
        return ! scanned.empty();
    }
    
    // Try scanning a string
    bool scan(const char *c, rstring_t *tok = NULL) {
        this->consume_leading_whitespace();
        rstring_t scanned = this->remaining.scan_string(c);
        if (tok) {
            *tok = scanned;
        }
        return ! scanned.empty();
    }

    
    bool scan_word(rstring_t *tok) {
        this->consume_leading_whitespace();
        /* A word may have embedded <...>. These may contain spaces. They may also abut: --foo<abc def>bar' is one word. So we use a loop. */
        rstring_t result;
        for (;;) {
            // Scan non-bracketed sequence. This may be empty (in which case the merge does nothing)
            result = result.merge(this->remaining.scan_while<char_is_valid_in_word>());
            // Scan bracketed sequence
            rstring_t bracket_start = this->remaining.scan_1_char('<');
            if (! bracket_start.empty()) {
                this->remaining.scan_while<char_is_valid_in_bracketed_word>();
                rstring_t bracket_end = this->remaining.scan_1_char('>');
                if (bracket_end.empty()) {
                    // TODO: report unclosed bracket
                } else {
                    // Merging the bracket ranges will include the contents
                    result = result.merge(bracket_start);
                    result = result.merge(bracket_end);
                }
            }
            // If we got an empty bracket range, then we're done; otherwise start again
            if (bracket_start.empty()) {
                break;
            }
        }
        *tok = result;
        return ! result.empty();
    }
    
    bool peek(const char *s) {
        const rstring_t saved = this->remaining;
        rstring_t tok;
        bool result = scan(s, &tok);
        this->remaining = saved;
        return result;
    }
    
    rstring_t peek_word() {
        const rstring_t saved = this->remaining;
        rstring_t tok;
        scan_word(&tok);
        this->remaining = saved;
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
            rstring_t bar;
            if (! first && ! this->scan('|', &bar)) {
                status = parsed_done;
                break;
            }
            status = try_parse_appending(&result->alternations);
            if (status == parsed_done) {
                if (! first) {
                    append_error(&this->errors, bar.start(), error_trailing_vertical_bar, "Trailing vertical bar");
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
        rstring_t word = this->peek_word();
        if (word.empty()) {
            // Nothing remaining to parse
            return parsed_done;
        }
        
        rstring_t::char_t c = word[0];
        if (c == '<') {
            return this->try_parse_auto(&result->variable);
        } else if (c == '-' && word.length() > 1) {
            // A naked '-', is to be treated as a fixed value
            return this->try_parse_auto(&result->option);
        } else {
            return this->try_parse_auto(&result->fixed);
        }
    }

    // Parse options clause
    parse_result_t parse(option_clause_t *result) {
        rstring_t word;
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
        rstring_t remaining = word;
        if (remaining.length() > 1 && remaining[0] == '-' && ! remaining.is_double_dash()) {
            // It's an option
            option_t opt_from_usage_section;
            if (! option_t::parse_from_string(&remaining, &opt_from_usage_section, &this->errors)) {
                return parsed_error;
            }
            assert(! opt_from_usage_section.best_name().empty());
            
            // See if we have a corresponding option in the options section
            const option_t *opt_from_options_section = NULL;
            for (size_t i=0; i < this->shortcut_options->size(); i++) {
                const option_t &test_op = this->shortcut_options->at(i);
                if (opt_from_usage_section.has_same_name(test_op)) {
                    opt_from_options_section = &test_op;
                    break;
                }
            }
            
            // We may have to parse a variable
            if (opt_from_usage_section.separator == option_t::sep_space) {
                // Looks like an option without a separator. See if the next token is a variable
                rstring_t next = this->peek_word();
                if (next.length() > 2 && next[0] == '<' && next[next.length() - 1] == '>') {
                    // It's a variable. See if we have a presence in options.
                    bool options_section_implies_no_value = (opt_from_options_section && opt_from_options_section->value.empty());
                    if (! options_section_implies_no_value) {
                        rstring_t variable;
                        bool scanned = this->scan_word(&variable);
                        assert(scanned); // Should always succeed, since we peeked at the word
                        word = word.merge(variable);
                        opt_from_usage_section.value = variable;
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
        rstring_t token;
        // Note that options must come before trying to parse it as a list, because "[options]" itself looks like a list
        if (this->scan("[options]", &token)) {
            result->production = 3;
            status = this->parse(&result->options_shortcut);
        } else if (this->scan('(', &token) || this->scan('[', &token)) {
            status = this->try_parse_auto(&result->alternation_list);
            if (status != parsed_error) {
                assert(token[0] == '(' || token[0] == '[');
                bool is_paren = (token[0] == '(');
                rstring_t close_token;
                if (this->scan(is_paren ? ')' : ']', &close_token)) {
                    result->production = is_paren ? 1 : 2;
                    parse(&result->opt_ellipsis); // never fails
                } else {
                    // No closing bracket or paren
                    if (is_paren) {
                        append_error(&this->errors, token.start(), error_missing_close_paren, "Missing ')' to match opening '('");
                    } else {
                        append_error(&this->errors, token.start(), error_missing_close_bracket, "Missing ']' to match opening '['");
                    }
                    status = parsed_error;
                }
            }
        } else if (this->scan("...", &token)) {
            append_error(&this->errors, token.start(), error_leading_ellipsis, "Ellipsis may only follow an expression");
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
                                   first->value == second->value &&
                                   ! first->name_types_overlap(*second));
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
    // Note the only reason this is safe is that string literals are immortal
    const char *storage = "command [options]";
    const rstring_t src(storage, strlen(storage));
    parse_one_usage(src, option_list_t(), this, NULL /* errors */);
}

bool parse_one_usage(const rstring_t &source, const option_list_t &shortcut_options, usage_t *out_usage, vector<error_t> *out_errors) {
    parse_context_t ctx(source, shortcut_options);
    parse_result_t status = ctx.parse(out_usage);
    assert(! (status == parsed_error && ctx.errors.empty()));
    if (out_errors) {
        out_errors->insert(out_errors->end(), ctx.errors.begin(), ctx.errors.end());
    }
    return status != parsed_error;
}

CLOSE_DOCOPT_IMPL /* namespace */

