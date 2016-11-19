#include <assert.h>
#include <stdint.h>
#include <string.h>
#include <algorithm>
#include <iostream>
#include <memory>
#include <numeric>
#include <set>
#include <vector>
#include "docopt_fish.h"
#include "docopt_fish_grammar.h"
#include "docopt_fish_types.h"

OPEN_DOCOPT_IMPL

#pragma mark -
#pragma mark Usage Grammar
#pragma mark -

// Helper to reset a unique_ptr to a new value constructed from the given
// arg
template <typename Contents, typename Arg>
void emplace_unique(std::unique_ptr<Contents> *ptr, Arg &&arg) {
    return ptr->reset(new Contents(std::forward<Arg>(arg)));
}

// Context passed around in our recursive descent parser
struct parse_context_t {
    static bool char_is_valid_in_word(rstring_t::char_t c) {
        const char *invalid = ".|()[], \t\n";
        const char *end = invalid + strlen(invalid);
        return std::find(invalid, end, c) == end;
    }

    // Range of source remains to be parsed
    // Note unowned pointer references in rstring_t. A parse context is stack
    // allocated and
    // transient.
    rstring_t remaining;

    const option_list_t *const shortcut_options;

    // Errors we generate
    vector<error_t> errors;

    parse_context_t(const rstring_t &usage, const option_list_t &shortcuts)
        : remaining(usage), shortcut_options(&shortcuts) {}

    void error(const rstring_t &tok, int code, const char *txt) {
        // Only bother to save the first error
        if (this->errors.empty()) {
            append_error(&this->errors, tok.start(), code, txt);
        }
    }

    // Consume leading whitespace.
    // Newlines are meaningful if their associated lines are indented the same or
    // less than
    // initial_indent.
    // If they are indented more, we swallow those.
    void consume_leading_whitespace() {
        this->remaining.scan_while<rstring_t::char_is_whitespace>();
    }

    // Try scanning a string
    bool scan(const char *c, rstring_t *tok) {
        this->consume_leading_whitespace();
        *tok = this->remaining.scan_string(c);
        return !tok->empty();
    }

    bool scan_word(rstring_t *tok) {
        this->consume_leading_whitespace();
        // A word may have embedded <>
        // These may abut: --foo<abc_def>bar' is one word.
        *tok = this->remaining.scan_while<char_is_valid_in_word>();
        return !tok->empty();
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
    template <typename T>
    inline bool try_parse_appending(vector<T> *vec) {
        T val;
        bool parsed = this->parse(&val);
        if (parsed) {
            vec->push_back(std::move(val));
        }
        return parsed;
    }

    // Given a pointer to a unique_ptr, try populating it
    // Parse a local and then, if successfull, move it into a p
    template <typename T>
    inline bool try_parse_unique(unique_ptr<T> *p) {
        T val;
        bool parsed = this->parse(&val);
        if (parsed) {
            emplace_unique(p, std::move(val));
        }
        return parsed;
    }

#pragma mark Parse functions

    bool parse(alternation_list_t *result) {
        size_t count = 0;
        // We expect only one alternation
        result->alternations.reserve(1);
        for (;;) {
            // Scan a vert bar if we're not first
            rstring_t bar;
            if (count > 0 && !this->scan("|", &bar)) {
                break;
            }
            if (!try_parse_appending(&result->alternations)) {
                if (count > 0) {
                    error(bar, error_trailing_vertical_bar, "Trailing vertical bar");
                }
                break;
            }
            count++;
        }

        // If we have an alternation like [-e | --erase], mark them as the same
        // option
        // This is kind of a hackish place to do this
        collapse_corresponding_options(result);

        return count > 0;
    }

    bool parse(expression_list_t *result) {
        bool got_something = false;
        while (try_parse_appending(&result->expressions)) got_something = true;
        return got_something;
    }

    // Entry point! Parse a usage_t.
    usage_t parse_usage() {
        usage_t result;
        bool scanned = this->scan_word(&result.prog_name);
        assert(scanned);  // else we should not have tried to parse this as a usage
        parse(&result.alternation_list);

        // We may get an empty alternation list if we are just the program name.
        // In that case, ensure we have at least one.
        if (result.alternation_list.alternations.empty()) {
            result.alternation_list.alternations.emplace_back();
        }
        return result;
    }

    // Parse ellipsis
    bool parse(opt_ellipsis_t *result) {
        result->present = this->scan("...", &result->ellipsis);
        return true;  // can't fail
    }

    // "Parse" [options]
    bool parse(options_shortcut_t *result) {
        result->present = true;
        return true;  // can't fail
    }

    // Parse a simple clause
    bool parse(simple_clause_t *result) {
        rstring_t word = this->peek_word();
        if (word.empty()) {
            // Nothing remaining to parse
            return false;
        }

        rstring_t::char_t c = word[0];
        if (c == '<') {
            return this->try_parse_unique(&result->variable);
        } else if (c == '-' && word.length() > 1) {
            // A naked '-', is to be treated as a fixed value
            return this->try_parse_unique(&result->option);
        } else {
            return this->try_parse_unique(&result->fixed);
        }
    }

    // Parse options clause
    bool parse(option_clause_t *result) {
        rstring_t word;
        if (!this->scan_word(&word)) {
            return false;
        }

        // TODO: handle --

        /* Hack to support specifying parameter names inline.

         Consider usage like this:
           "usage: prog [-m <msg>]"

        There's two ways we can interpret this:
         1. An optional -m flag, followed by a positional parameter
         2. An -m option whose value is <msg>

        The Python reference docopt resolves this ambiguity by looking at the Options: section. If
        it finds a declaration of -m with a variable, then <msg> is assumed to be the value
        (interpretation #2); otherwise it's a positional (interpretation #1).

        But requiring a required positional after a flag seems very unlikely, so in this version we
        always use interpretation #2. We do this by treating it as one token '-m <msg>'. Note this
        token contains a space.

        One exception is if a delimeter is found, e.g.:

        "usage: prog [-m=<foo> <msg>]"

        Now <foo> is the value of the option m, and <msg> is positional.

        The second exception is if the option is also found in the Options: section without a
        variable:

            usage: prog -m <msg>
            options: -m

        Then we ignore the option.

        Also, if you really want interpretation #1, you can use parens:

           "usage: prog [-m (<msg>)]"
        */
        rstring_t remaining = word;
        if (remaining.length() > 1 && remaining[0] == '-' && !remaining.is_double_dash()) {
            // It's an option
            option_t opt_from_usage_section;
            if (!option_t::parse_from_string(&remaining, &opt_from_usage_section, &this->errors)) {
                // must have an error in this case
                assert(!this->errors.empty());
                return false;
            }
            assert(!opt_from_usage_section.best_name().empty());

            // See if we have a corresponding option in the options section
            const option_t *opt_from_options_section = nullptr;
            for (const option_t &test_op : *this->shortcut_options) {
                if (opt_from_usage_section.has_same_name(test_op)) {
                    opt_from_options_section = &test_op;
                    break;
                }
            }

            // We may have to parse a variable
            if (opt_from_usage_section.separator == option_t::sep_space) {
                // Looks like an option without a separator. See if the next token is a
                // variable
                rstring_t next = this->peek_word();
                if (next.length() > 2 && next[0] == '<' && next[next.length() - 1] == '>') {
                    // It's a variable. See if we have a presence in options.
                    bool options_section_implies_no_value =
                        (opt_from_options_section && opt_from_options_section->value.empty());
                    if (!options_section_implies_no_value) {
                        rstring_t variable;
                        bool scanned = this->scan_word(&variable);
                        assert(scanned);  // Should always succeed, since we peeked at the word
                        word = word.merge(variable);
                        opt_from_usage_section.value = variable;
                    }
                }
            }

            // Use the option from the Options section in preference to the one from
            // the Usage
            // section, since the options one has more information like the
            // corresponding long name
            // and description
            result->word = word;
            result->option =
                opt_from_options_section ? *opt_from_options_section : opt_from_usage_section;
        }
        return true;
    }

    // Parse a fixed argument
    bool parse(fixed_clause_t *result) {
        // TODO: handle invalid commands like foo<bar>
        return this->scan_word(&result->word);
    }

    // Parse a variable argument
    bool parse(variable_clause_t *result) {
        // TODO: handle invalid variables like foo<bar>
        return this->scan_word(&result->word);
    }

    // Parse a general expression
    bool parse(expression_t *result) {
        rstring_t token;
        bool success = false;
        // Note that options must come before trying to parse it as a list, because
        // "[options]"
        // itself looks like a list
        if (this->scan("[options]", &token)) {
            result->production = 3;
            success = this->parse(&result->options_shortcut);
        } else if (this->scan("(", &token) || this->scan("[", &token)) {
            if (!this->try_parse_unique(&result->alternation_list)) {
                error(token, error_empty_bracket_paren, "Expected an expression inside");
            }
            bool is_paren = (token[0] == '(');
            rstring_t close_token;
            if (this->scan(is_paren ? ")" : "]", &close_token)) {
                result->production = is_paren ? 1 : 2;
                success = parse(&result->opt_ellipsis);  // never fails
            } else {
                // No closing bracket or paren
                if (is_paren) {
                    error(token, error_missing_close_paren, "Missing ')' to match opening '('");
                } else {
                    error(token, error_missing_close_bracket, "Missing ']' to match opening '['");
                }
            }
        } else if (this->scan("...", &token)) {
            error(token, error_leading_ellipsis, "Ellipsis may only follow an expression");
        } else if (this->peek("|")) {
            // End of an alternation list, nothing to do
            success = false;
        } else {
            // Simple clause
            success = try_parse_unique(&result->simple_clause);
            if (success) {
                result->production = 0;
                parse(&result->opt_ellipsis);  // never fails except on error
            }
        }
        return success;
    }

    // Given an expression list, if it wraps a single option, return a pointer to that option. Else
    // return nullptr.
    static option_t *single_option(expression_list_t *list) {
        if (list->expressions.size() != 1) {
            return nullptr;
        }
        expression_t *expr = &list->expressions[0];
        simple_clause_t *simple_clause = expr->simple_clause.get();
        option_clause_t *option_clause = simple_clause ? simple_clause->option.get() : nullptr;
        return option_clause ? &option_clause->option : nullptr;
    }

    /* For example:
     usage: prog [-e | --erase]
     prog [-a <name> | --add <name>]

     Here we need to mark -e's corresponding long name as --erase, and same for
     -a/--add.
     This applies if we have exactly two options.
     */
    void collapse_corresponding_options(alternation_list_t *list) {
        assert(list != nullptr);
        // Must have exactly 2 alternations
        if (list->alternations.size() != 2) {
            return;
        }
        option_t *first = single_option(&list->alternations[0]);
        option_t *second = single_option(&list->alternations[1]);

        // Both options must be non-NULL, and they must not have overlapping name types, and their
        // values must agree (perhaps both empty)
        bool options_correspond =
            (first != nullptr && second != nullptr && first->value == second->value &&
             !first->name_types_overlap(*second));
        if (options_correspond) {
            // Merge them. Note: this merge_from call writes deep into our tree!
            // Then delete the second alternation
            first->merge_from(*second);
            list->alternations.pop_back();
            assert(list->alternations.size() == 1);
        }
    }
};

usage_t parse_one_usage(const rstring_t &source, const option_list_t &shortcut_options,
                        vector<error_t> *out_errors) {
    parse_context_t ctx(source, shortcut_options);
    usage_t result = ctx.parse_usage();

    // Return errors
    if (out_errors) {
        std::move(ctx.errors.begin(), ctx.errors.end(), std::back_inserter(*out_errors));
    }

    // Be careful not to return the usage in case of error
    // It may be an invalid usage, i.e. not respect the invariants
    // expected in match()
    if (ctx.errors.empty()) {
        return result;
    } else {
        return usage_t();
    }
}

// Helper to build an rstring_t from a constant C string
// Note this can't be a function since it has a dependency on DOCOPT_USE_WCHAR
// Also note this is safe because string literals are immortal
#if DOCOPT_USE_WCHAR
#define CONST_RSTRING(x) rstring_t(L##x, wcslen(L##x))
#else
#define CONST_RSTRING(x) rstring_t(x, strlen(x))
#endif

usage_t usage_t::make_default() {
    const rstring_t src = CONST_RSTRING("command [options]");
    return parse_one_usage(src, option_list_t(), nullptr /* errors */);
}

// Support for the annotated-options path of docopt
// Given a list of variables, construct a synthetic usage_t
// The usage_t should contain an alternation list, one entry per variable
// If include_options_shortcut is set,then each alternation also gets an
// [options] expression,
// and there is also an alternation which is just the options expression (i.e.
// no variable)
usage_t usage_t::build_from_variables(const std::vector<rstring_t> &variables,
                                      bool include_options_shortcut) {
    // Note this is safe because string literals are immortal
    usage_t usage;
    usage.prog_name = CONST_RSTRING("command");

    vector<expression_list_t> &alternations = usage.alternation_list.alternations;
    alternations.reserve(variables.size() + (include_options_shortcut ? 1 : 0));

    // Helper to build an options expression_t
    auto make_options_expr = []() -> expression_t {
        expression_t options_expr;
        options_expr.production = 3;
        options_expr.options_shortcut.present = true;
        return options_expr;
    };

    // Helper to build a variable expression
    auto make_variable_expr = [](const rstring_t &var_str) -> expression_t {
        simple_clause_t sclause;
        emplace_unique(&sclause.variable, variable_clause_t{var_str});
        expression_t expr;
        expr.production = 0;
        emplace_unique(&expr.simple_clause, std::move(sclause));
        return expr;
    };

    // Set variable clauses
    for (const rstring_t &var : variables) {
        expression_list_t exprs(make_variable_expr(var));
        if (include_options_shortcut) {
            exprs.expressions.push_back(make_options_expr());
        }
        alternations.push_back(std::move(exprs));
    }

    // Also include just an options clause
    if (include_options_shortcut) {
        alternations.emplace_back(expression_list_t(make_options_expr()));
    }
    return usage;
}

CLOSE_DOCOPT_IMPL /* namespace */
