#include "docopt_fish.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <algorithm>
#include <iostream>
#include <memory>
#include <numeric>
#include <set>
#include "docopt_fish_grammar.h"
#include "docopt_fish_types.h"

using std::shared_ptr;

OPEN_DOCOPT_IMPL

static const size_t npos = string_t::npos;

typedef std::vector<rstring_t> rstring_list_t;
typedef std::vector<error_t> error_list_t;
typedef std::vector<size_t> index_list_t;

// Class representing a metadata map. Keys are options and variables.
typedef base_metadata_t<rstring_t> rmetadata_t;
typedef std::map<rstring_t, rmetadata_t> metadata_map_t;

// String helper
// Handles both wide and narrow string_t
static inline string_t to_string(const char *s) {
    return string_t(s, s + strlen(s));
}

static inline std::basic_ostream<string_t::value_type> &errstream() {
#if DOCOPT_USE_WCHAR
    return std::wcerr;
#else
    return std::cerr;
#endif
}

// Little helper function
bool is_double_dash(const string_t &str) {
    return str.size() == 2 && str[0] == '-' && str[1] == '-';
}

// This represents an error in argv, i.e. the docopt description was OK but a
// parameter contained an
// error
static void append_argv_error(error_list_t *errors, size_t arg_idx, int code, const char *txt,
                              size_t pos_in_arg = 0) {
    append_error(errors, pos_in_arg, code, txt, arg_idx);
}

// This represents an error in the docopt specification itself
// The token is a substring of the docopt spec, and its location is used to
// determine the error
// position
static void append_docopt_error(error_list_t *errors, const rstring_t &token, int code,
                                const char *txt) {
    append_error(errors, token.offset(), code, txt, -1);
}

/* Parsing helpers */
template <char T>
bool it_equals(rstring_t::char_t c) {
    return c == T;
}

bool char_is_valid_in_parameter(rstring_t::char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

bool char_is_valid_in_variable_name(rstring_t::char_t c) {
    const char *invalid = "|()[]>\t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

bool char_is_space(rstring_t::char_t c) {
    return c == ' ';
}

// Given an inout string, parse out an option and return it by reference.
// Update the string to reflect the number of characters used.
bool option_t::parse_from_string(rstring_t *remaining, option_t *result, error_list_t *out_errors) {
    assert(!remaining->empty());
    error_list_t errors;

    // An option is one or more dashes, the option name, maybe space and/or equals
    rstring_t leading_dashes, name, space_separator, equals;
    std::tie(leading_dashes, name, space_separator, equals, std::ignore) =
        remaining->scan_multiple<it_equals<'-'>, char_is_valid_in_parameter, char_is_space,
                                 it_equals<'='>, char_is_space>();

    // Now maybe scan the variable name as <var_name>
    rstring_t variable_name, open_sign, close_sign;
    open_sign = remaining->scan_1_char('<');
    const bool has_variable = !open_sign.empty();
    if (has_variable) {
        variable_name = remaining->scan_while<char_is_valid_in_variable_name>();
        close_sign = remaining->scan_1_char('>');
    }

    // Validate our fields
    const size_t dash_count = leading_dashes.length();
    assert(dash_count > 0);
    if (dash_count > 2) {
        append_docopt_error(&errors, leading_dashes, error_excessive_dashes, "Too many dashes");
    }

    if (name.empty()) {
        append_docopt_error(&errors, name, error_invalid_option_name, "Missing option name");
    }

    if (equals.length() > 1) {
        append_docopt_error(&errors, equals, error_excessive_equal_signs, "Too many equal signs");
    }

    // Report an error for cases like '--foo='
    if (!has_variable && !equals.empty()) {
        append_docopt_error(&errors, equals, error_invalid_variable_name,
                            "Missing variable for this assignment");
    }

    // Validate variable
    if (has_variable) {
        if (variable_name.empty()) {
            append_docopt_error(&errors, variable_name, error_invalid_variable_name,
                                "Missing variable name");
        } else if (close_sign.empty()) {
            append_docopt_error(&errors, open_sign, error_invalid_variable_name,
                                "Missing '>' to match this '<'");
        } else if (!remaining->empty() && char_is_valid_in_parameter(remaining->at(0))) {
            // Next character is not whitespace and not the end of the string
            append_docopt_error(&errors, *remaining, error_invalid_variable_name,
                                "Extra stuff after closing '>'");
        }
    }

    // Determine the separator type
    // If we got an equals range, it's something like 'foo = <bar>' or
    // 'foo=<bar>'. The separator is
    // equals and the space is ignored.
    // Otherwise, the space matters: 'foo <bar>' is space-separated, and
    // 'foo<bar>' has no separator
    // Hackish: store sep_space for options without a variable
    option_t::separator_t separator = option_t::sep_space;
    if (has_variable) {
        if (!equals.empty()) {
            separator = option_t::sep_equals;
        } else if (!space_separator.empty()) {
            separator = option_t::sep_space;
        } else {
            separator = option_t::sep_none;
        }
    }

    // Generate an error on long options with no separators (--foo<bar>). Only
    // short options support
    // these.
    if (separator == option_t::sep_none && (dash_count > 1 || name.length() > 1)) {
        append_docopt_error(&errors, name, error_bad_option_separator,
                            "Long options must use a space or equals separator");
    }

    // Determine the type
    name_type_t type;
    if (dash_count > 1) {
        type = double_long;
    } else if (name.length() > 1) {
        type = single_long;
    } else {
        type = single_short;
    }

    bool success = errors.empty();
    if (success) {
        // Build the variable like '<foo>'
        // If we don't have one, this will be empty
        const rstring_t variable = open_sign.merge(variable_name).merge(close_sign);
        *result = option_t(type, leading_dashes.merge(name), variable, separator);
    }
    std::move(errors.begin(), errors.end(), std::back_inserter(*out_errors));
    return success;
}

option_t option_t::parse_from_argument(const string_t &str, option_t::name_type_t type) {
    assert(!str.empty());
    assert(str.at(0) == '-');

    rstring_t remaining(str);

    // Get the name part
    const rstring_t dashes = remaining.scan_while<it_equals<'-'>>();
    rstring_t name, value;
    option_t::separator_t separator = option_t::sep_space;
    if (type == single_short) {
        // Short option, perhaps with unseparated value
        // here the name is just the next character (if any)
        // and the value is anything after it
        if (!remaining.empty()) {
            name = remaining.substr(0, 1);
            value = remaining.substr_from(1);
            separator = option_t::sep_none;
        }
    } else {
        // Long option
        name = remaining.scan_while<char_is_valid_in_parameter>();

        // Check to see if there's an = sign
        // If we got an equals sign, the rest is the value
        // It can have any character at all, since it's coming from the argument,
        // not from the usage spec
        const rstring_t equals = remaining.scan_1_char('=');
        if (!equals.empty()) {
            value = remaining;
            separator = option_t::sep_equals;
        }
    }

    // Return the option
    return option_t(type, dashes.merge(name), value, separator);
}

/* Helper class for pretty-printing */
class node_dumper_t : public node_visitor_t<node_dumper_t> {
    unsigned int depth;

    std::vector<string_t> lines;

    node_dumper_t() : depth(0) {}

   public:
    template <typename NODE_TYPE>
    void accept(const NODE_TYPE &node) {
        string_t result(2 * depth, ' ');
        result.append(to_string(node.name().c_str()));
        lines.push_back(result);
    }

    /* Override of visit() to bump the depth */
    template <typename NODE_TYPE>
    void visit(const NODE_TYPE &t) {
        depth += 1;
        node_visitor_t<node_dumper_t>::visit(t);
        depth -= 1;
    }

    void accept(const rstring_t &t1) {
        if (!t1.empty()) {
            string_t result(2 * depth, ' ');
            const string_t quote(1, '\'');

            result += quote + t1.std_string() + quote;

            char buff[32];
            if (t1.length() == 1) {
                snprintf(buff, sizeof buff, "{%lu}", t1.offset());
            } else {
                snprintf(buff, sizeof buff, "{%lu-%lu}", t1.offset(), t1.length());
            }
            result.append(to_string(buff));
            lines.push_back(result);
        }
    }

    template <typename NODE_TYPE>
    static string_t dump_tree(const NODE_TYPE &node) {
        node_dumper_t dumper;
        dumper.begin(node);
        string_t result;
        for (const string_t &line : dumper.lines) {
            result.append(line);
            result.push_back('\n');
        }
        return result;
    }
};

/* Helper class for collecting clauses from a tree */
struct option_collector_t : public node_visitor_t<option_collector_t> {
    option_list_t options;

    // The requested types we capture
    void accept(const option_clause_t &node) {
        options.push_back(node.option);
    }

    // Other types we ignore
    template <typename IGNORED_TYPE>
    void accept(const IGNORED_TYPE &t UNUSED) {}
};

/* Helper to efficiently iterate over lines of a string 'base'. inout_line
 * should be initially
 * empty. On return, it will contain the line, with its end pointing just after
 * the trailing
 * newline, or possibly at the end. Returns true if a line was returned, false
 * if we reached the
 * end. */
static bool get_next_line(const string_t &base, rstring_t *inout_line) {
    assert(inout_line != nullptr);
    // Start at the end of the last line, or 0 if this is the first
    const size_t line_start = inout_line->end_offset();
    assert(line_start <= base.size());
    if (line_start == base.size()) {
        // Line exhausted
        return false;
    }

    // Take everything up to (and including) the newline if we have one
    size_t newline = base.find('\n', line_start);
    size_t line_end = (newline == string_t::npos ? base.size() : newline + 1);
    assert(line_end > line_start);
    *inout_line = rstring_t(base, line_start, line_end - line_start);
    // Empty lines are impossible
    assert(!inout_line->empty());
    return true;
}

/* A resolved option references an option in argv */
struct resolved_option_t {
    // The option referenced by this
    option_t option;

    // The index of the name portion of the option, in argv
    size_t name_idx_in_argv;

    // The index of the argument where the value was found. npos for none.
    size_t value_idx_in_argv;

    // The range within that argument where the value was found. This will be the
    // entire string if
    // the argument is separate (--foo bar) but will be the portion after the
    // equals if not
    // (--foo=bar, -Dfoo)
    rstring_t value_in_arg;

    resolved_option_t(const option_t &opt, size_t name_idx, size_t val_idx, const rstring_t &value)
        : option(opt),
          name_idx_in_argv(name_idx),
          value_idx_in_argv(val_idx),
          value_in_arg(value) {}
};
typedef std::vector<resolved_option_t> resolved_option_list_t;

/* List of usages */
typedef std::vector<usage_t> usage_list_t;

/* Collects options, i.e. tokens of the form --foo */
static option_list_t collect_options(const usage_list_t &usages) {
    option_collector_t collector;
    for (const usage_t &usage : usages) {
        collector.begin(usage);
    }
    return std::move(collector.options);
}

/* A positional argument */
struct positional_argument_t {
    size_t idx_in_argv;

    explicit positional_argument_t(size_t idx) : idx_in_argv(idx) {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;

/* Given an option spec, that extends from the initial - to the end of the
 * description, parse out an
 * option. Store descriptions in the given metadata. It may have multiple names.
 */
static option_t parse_one_option_spec(const rstring_t &spec, metadata_map_t *metadata,
                                      error_list_t *errors) {
    assert(!spec.empty() && spec[0] == '-');
    const size_t end = spec.length();
    option_t result;

    // Look for two spaces. Those separate the description.
    size_t options_end = spec.find("  ");
    if (options_end > end) {
        options_end = end;  // no description
    }

    // Determine the description range (possibly empty). Trim leading and trailing
    // whitespace
    rstring_t description = spec.substr_from(options_end).trim_whitespace();

    // Parse out a "default:" value.
    if (!description.empty()) {
        // TODO: handle the case where there's more than one
        const char *default_prefix = "[default:";
        size_t default_prefix_loc = description.find_case_insensitive(default_prefix);
        if (default_prefix_loc != rstring_t::npos) {
            rstring_t default_value =
                description.substr_from(default_prefix_loc + strlen(default_prefix))
                    .trim_whitespace();

            // Find the closing ']'
            size_t default_value_end = default_value.find("]");
            if (default_value_end == rstring_t::npos) {
                append_docopt_error(errors, default_value, error_missing_close_bracket_in_default,
                                    "Missing ']' to match opening '['");
            } else {
                result.default_value = default_value.substr(0, default_value_end);
            }
        }
    }

    // Parse the options portion
    rstring_t remaining = spec.substr(0, options_end);
    remaining.scan_while<char_is_space>();
    while (!remaining.empty()) {
        if (remaining[0] != '-') {
            append_docopt_error(errors, remaining, error_invalid_option_name, "Not an option");
            break;
        }

        option_t opt;
        if (!option_t::parse_from_string(&remaining, &opt, errors)) {
            // Failed to get an option, give up
            break;
        }
        result.merge_from(opt);

        // Skip over commas, which separate arguments
        remaining.scan_multiple<char_is_space, it_equals<','>, char_is_space>();
    }

    // Store any description in the metadata
    if (!description.empty()) {
        for (size_t i = 0; i < option_t::NAME_TYPE_COUNT; i++) {
            rstring_t name = result.names[i];
            if (!name.empty()) {
                (*metadata)[name].description = description;
            }
        }
    }

    return result;
}

/* Returns a header in the given string, or an empty string if none. We are
 * considered a header if
 * we contain a colon, and only space / alpha text before it. */
static rstring_t find_header(const rstring_t &src) {
    rstring_t result;
    for (size_t i = 0; i < src.length(); i++) {
        rstring_t::char_t c = src[i];
        if (c == ':') {
            result = src.substr(0, i + 1);
            break;
        } else if (c != ' ' && !isalnum(c)) {
            // Not a header
            break;
        }
    }
    return result;
}

// Given a variable spec, parse out a command
// A specification look like this:
// <pid> stuff
// Return the variable name <pid> and the command stuff by reference
static bool parse_one_variable_command_spec(const rstring_t &spec, rstring_t *out_variable_name,
                                            rstring_t *out_command, error_list_t *out_errors) {
    bool result = false;
    assert(!spec.empty() && spec[0] == '<');
    const size_t close_bracket = spec.find(">");
    if (close_bracket == rstring_t::npos) {
        append_docopt_error(out_errors, spec, error_missing_close_variable,
                            "No > to balance this <");
    } else {
        assert(close_bracket < spec.length());
        *out_variable_name = spec.substr(0, close_bracket + 1).trim_whitespace();
        *out_command = spec.substr_from(close_bracket + 1).trim_whitespace();
        result = true;
    }
    return result;
}

// Given a string 'src' and a whitespace-trimmed substring trimmed_src, compute
// how much trimmed_src
// is indented. Tabs are treated as 4 spaces. newlines are unexpected, and
// treated as one space.
static size_t compute_indent(const rstring_t &src, const rstring_t &trimmed_src) {
    assert(trimmed_src.offset() >= src.offset() && trimmed_src.end_offset() <= src.end_offset());
    const size_t tabstop = 4;
    // Walk over the prefix of src, up to trimmed_src
    size_t result = 0;
    size_t length = trimmed_src.offset() - src.offset();
    for (size_t i = 0; i < length; i++) {
        if (src.at(i) != '\t') {
            // not a tab
            result += 1;
        } else {
            // is a tab. Round up to the next highest multiple of tabstop.
            // If we're already a multiple of tabstop, we want to go bigger.
            result = (result + tabstop) / tabstop * tabstop;
        }
    }
    return result;
}

// Given a list of options, verify that any duplicate options are in agreement,
// and remove all but
// one.
// TODO: Can we make this not N^2 without heap allocation?
static void uniqueize_options(option_list_t *options, bool error_on_duplicates,
                              error_list_t *errors) {
    // Maintain an outer cursor. For each option, loop over the remainder,
    // deleting those that share
    // a name
    // Grab the best description as we go
    // We "delete" from the middle of the vector by moving the last element into
    // the slot, and then
    // decrementing the length
    size_t options_count = options->size();  // note this changes as we go
    for (size_t outer = 0; outer < options_count; outer++) {
        option_t *representative = &options->at(outer);

        // Find all options that share a name with this representative
        // Determine which one is best
        for (size_t match_cursor = outer + 1; match_cursor < options_count; match_cursor++) {
            option_t *candidate = &options->at(match_cursor);
            if (!representative->has_same_name(*candidate)) {
                continue;
            }

            // Ok, we know that candidate has the same name as the best match
            // Generate an error if we're supposed to
            // TODO: verify agreement in the parameters, etc.
            // Then we copy the description over if it's better, then "erase" the
            // candidate
            // That will cause us to skip over it later
            if (error_on_duplicates) {
                // Generate an error, and then continue on
                append_docopt_error(errors, candidate->best_name(),
                                    error_option_duplicated_in_options_section,
                                    "Option specified more than once");
            }

            // "Delete" candidate by overwriting it with the last value, and
            // decrementing the count
            *candidate = std::move(options->back());
            options->pop_back();
            options_count -= 1;
            match_cursor -= 1;  // have to re-evaluate this value
        }
    }
}

/* Transient stack-allocated data associated with separating argv */
struct argv_separation_state_t {
    const string_list_t &argv;
    const option_list_t &options;
    parse_flags_t flags;
    size_t idx = 0;
    bool saw_double_dash = false;

    argv_separation_state_t(const string_list_t &argv_, const option_list_t &options_,
                            parse_flags_t flags_)
        : argv(argv_), options(options_), flags(flags_) {}

    const string_t &arg() const {
        return this->argv.at(this->idx);
    }

    bool arg_has_prefix(const char *p) const {
        const string_t &arg = this->arg();
        const size_t len = strlen(p);
        return arg.size() >= len && std::equal(p, p + len, arg.begin());
    }

    bool has_double_dash_at(size_t idx) const {
        return idx < this->argv.size() && is_double_dash(this->argv.at(idx));
    }

    // Returns the list of options satisfying some predicate
    template <typename UnaryFunc>
    std::vector<option_t> filter_options(const UnaryFunc &pred) const {
        std::vector<option_t> result;
        copy_if(this->options.begin(), this->options.end(), std::back_inserter(result), pred);
        return result;
    }
};

// Parse either a long value (type is single_long or double_long),
// or an unseparated short value like -DNDEBUG (type is single_short)
// We do NOT handle separated short values (-D NDEBUG) here;
// those are handled in parse_short
static bool parse_long_or_unseparated_short(argv_separation_state_t *st, option_t::name_type_t type,
                                            resolved_option_list_t *out_result,
                                            error_list_t *out_errors, rstring_t *out_suggestion) {
    const string_t &arg = st->arg();
    assert(st->arg_has_prefix(type == option_t::double_long ? "--" : "-"));

    // If strict_separators is set, then we require that the option have sep_none
    // If not set, then we don't care if the separators match
    const bool relaxed_separators = !(st->flags & flag_short_options_strict_separators);

    // Parse the argument into an 'option'. Note that this option does not appear
    // in the options list because its range reflects the string in the argument.
    // TODO: Need to distinguish between equivalent ways of specifying parameters
    // (--foo=bar and --foo bar)
    option_t arg_as_option = option_t::parse_from_argument(arg, type);
    const rstring_t arg_name = arg_as_option.names[type];
    assert(!arg_name.empty());

    // Get list of matching options.
    // Short options must have a value to match, else we handle them in parse_short
    // TODO: can eliminate matches array entirely, just use a single index
    const option_list_t matches = st->filter_options([&](const option_t &opt) {
        bool does_match = opt.names[type] == arg_name;
        if (does_match && type == option_t::single_short) {
            does_match = opt.has_value() &&
                         (relaxed_separators || opt.separator == option_t::sep_none);
        }
        return does_match;
    });

    bool success = false;
    size_t match_count = matches.size();
    // Our option de-duplication ensures we should never have more than one match
    assert(match_count <= 1);
    if (match_count < 1) {
        append_argv_error(out_errors, st->idx, error_unknown_option, "Unknown option");
    } else {
        bool errored = false;
        assert(match_count == 1);
        const option_t &match = matches.at(0);

        // Ensure the option and argument agree on having a value
        rstring_t value;
        const size_t name_idx = st->idx;
        size_t arg_index = npos;
        if (match.has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar (long) or -DNDEBUG (short)
                // The range is the value portion; the index is the same as our
                // argument.
                // TODO: suggestions
                value = arg_as_option.value;
                arg_index = st->idx;
            } else if (match.value_is_optional) {
                // No option was specified, but we may have an optional value
                // Give the optional value as a suggestion
                if ((st->flags & flag_generate_suggestions) && out_suggestion != nullptr) {
                    *out_suggestion = match.value;
                }
            } else {
                // The arg was (hopefully) specified as --foo bar
                // The index is of the next argument, and the range is the entire argument
                // Maybe do double-dash
                if (st->has_double_dash_at(st->idx + 1)) {
                    st->saw_double_dash = true;
                    st->idx += 1;
                }
                if (st->idx + 1 < st->argv.size()) {
                    st->idx += 1;
                    arg_index = st->idx;
                    value = rstring_t(st->argv.at(arg_index));
                } else if ((st->flags & flag_generate_suggestions) && out_suggestion != nullptr) {
                    // We are at the last argument, and we expect a value. Return the
                    // value as a suggestion.
                    *out_suggestion = match.value;
                    errored = true;
                } else {
                    append_argv_error(out_errors, st->idx, error_option_has_missing_argument,
                                      "Option expects an argument");
                    errored = true;
                }
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            append_argv_error(out_errors, st->idx, error_option_unexpected_argument,
                              "Option does not expect an argument");
            errored = true;
        }

        // If we want strict separators, check for separator agreement
        if (!errored && (st->flags & flag_short_options_strict_separators)) {
            if (arg_as_option.separator != match.separator) {
                // TODO: improve this error
                append_argv_error(out_errors, st->idx, error_wrong_separator,
                                  "Option expects a different separator");
                errored = true;
            }
        }

        if (!errored) {
            out_result->emplace_back(match, name_idx, arg_index, value);
            st->idx += 1;
            success = true;
        }
    }
    return success;
}

// Given a list of short options, parse out arguments
// There may be multiple arguments, e.g. 'tar -xc'
// Only the last option may have an argument, e.g. 'tar -xcf somefile'
static bool parse_short(argv_separation_state_t *st, resolved_option_list_t *out_result,
                        error_list_t *out_errors, rstring_t *out_suggestion) {
    const string_t &arg = st->arg();
    assert(st->arg_has_prefix("-"));
    assert(arg.length() > 1);  // must not be just a single dash
    bool errored = false;
    bool last_option_has_argument = false;

    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> options_for_argument;

    std::vector<option_t> matches;
    // walk over the characters in the argument, skipping the leading dash
    for (size_t idx_in_arg = 1; idx_in_arg < arg.length() && !errored; idx_in_arg++) {
        // Get list of short options matching this resolved option.
        const rstring_t::char_t short_char = arg.at(idx_in_arg);
        matches.clear();
        for (const option_t &opt : st->options) {
            const rstring_t &name = opt.names[option_t::single_short];
            // This is a short option (-D) so we check the second character (idx 1)
            if (name.length() > 1 && name[1] == short_char) {
                matches.push_back(opt);
            }
        }

        size_t match_count = matches.size();
        // We should catch all duplicates during the preflight phase
        assert(match_count <= 1);
        if (match_count < 1) {
            append_argv_error(out_errors, st->idx, error_unknown_option, "Unknown short option",
                              idx_in_arg);
            errored = true;
        } else {
            // Just one match, add it to the global array
            options_for_argument.push_back(matches.at(0));
        }
    }

    if (!errored) {
        // Now we have all of the short options that this argument represents. No
        // option is allowed
        // to have a value, except possibly last one.
        for (size_t i = 0; i < options_for_argument.size(); i++) {
            const option_t &opt = options_for_argument.at(i);
            if (opt.has_value()) {
                if (i + 1 == options_for_argument.size()) {
                    // This is the last option
                    last_option_has_argument = true;
                } else {
                    // This is not the last option
                    // This i+1 is the position in the argument and needs some
                    // explanation. Since we
                    // have a leading dash and then an argument, which is parsed into
                    // short options
                    // - one per character, including the dash. Hence we can map from
                    // index-in-option to index-in-argument, unless there was an unknown
                    // option
                    // error above. In that case this will be wrong (but we typically only
                    // show the
                    // first error anyways).
                    append_argv_error(out_errors, st->idx, error_option_unexpected_argument,
                                      "Option may not have a value unless it is the last option",
                                      i + 1);
                }
            }
        }
    }

    // If we have an argument, determine its index
    rstring_t value_for_last_option;
    const size_t name_idx = st->idx;
    size_t val_idx_for_last_option = npos;
    if (!errored && last_option_has_argument) {
        // We don't support -f=bar style. I don't know of any commands that use
        // this.
        // TODO: support delimiter-free style (gcc -Dmacro=something)
        // Handle possible double-dash
        if (st->has_double_dash_at(st->idx + 1)) {
            st->saw_double_dash = true;
            st->idx += 1;
        }

        if (st->idx + 1 < st->argv.size()) {
            val_idx_for_last_option = st->idx + 1;
            value_for_last_option = rstring_t(st->argv.at(st->idx + 1));
        } else if ((st->flags & flag_generate_suggestions) && out_suggestion != nullptr) {
            // We are at the last argument, and we expect a value. Return the value as
            // a suggestion.
            const option_t &match = options_for_argument.back();
            *out_suggestion = match.value;
            errored = true;
        } else {
            append_argv_error(out_errors, st->idx, error_option_has_missing_argument,
                              "Option expects an argument");
            errored = true;
        }
    }

    if (!errored) {
        // Construct resolved options
        for (size_t i = 0; i < options_for_argument.size(); i++) {
            // Most options have no value.
            size_t val_idx = npos;
            rstring_t value;
            if (i + 1 == options_for_argument.size() && last_option_has_argument) {
                // This is the last option
                value = value_for_last_option;
                val_idx = val_idx_for_last_option;
            }
            const option_t &opt = options_for_argument.at(i);
            out_result->emplace_back(opt, name_idx, val_idx, value);
        }

        // Update the index
        st->idx += (last_option_has_argument ? 2 : 1);
    }
    return !errored;
}

// The Python implementation calls this "parse_argv"
// Given an argv list, a set of options, and a set of flags,
// parse the argv into a set of positionals, options, errors, and optionally a suggestion
// last_argument_is_partial tracks whether the last argument is being used for suggestions
static void separate_argv_into_options_and_positionals(
    const string_list_t &argv, const option_list_t &options, parse_flags_t flags,
    positional_argument_list_t *out_positionals, resolved_option_list_t *out_resolved_options,
    error_list_t *out_errors, rstring_t *out_suggestion = nullptr) {
    // double_dash means that all remaining values are arguments
    argv_separation_state_t st(argv, options, flags);
    while (st.idx < argv.size()) {
        if (st.saw_double_dash) {
            // double-dash means everything remaining is positional
            out_positionals->emplace_back(st.idx);
            st.idx += 1;
        } else if (st.has_double_dash_at(st.idx)) {
            // Literal --. The remaining arguments are positional.
            st.saw_double_dash = true;
            st.idx += 1;
        } else if (st.arg_has_prefix("--")) {
            // Leading long option
            if (parse_long_or_unseparated_short(&st, option_t::double_long, out_resolved_options,
                                                out_errors, out_suggestion)) {
                // parse_long will have updated st.idx and out_resolved_options
            } else {
                // This argument is unused
                // We have to update idx
                st.idx += 1;
            }
        } else if (st.arg_has_prefix("-") && st.arg().length() > 1) {
            /* An option with a leading dash, like -foo
             This can be a lot of different things:
             1. A combined short option: tar -cf ...
             2. A long option with a single dash: -std=c++
             3. A short option with a value: -DNDEBUG
             Try to parse it as a long option; if that fails try to parse it as a
             short option.
             We cache the errors locally so that failing to parse it as a long option
             doesn't report
             an error if it parses successfully as a short option. This may result in
             duplicate
             error messages.
             */
            error_list_t local_long_errors, local_short_errors;
            if (parse_long_or_unseparated_short(&st, option_t::single_long, out_resolved_options,
                                                &local_long_errors, out_suggestion)) {
                // parse_long succeeded
            } else if (parse_long_or_unseparated_short(&st, option_t::single_short,
                                                       out_resolved_options, &local_short_errors,
                                                       out_suggestion)) {
                // parse_unseparated_short will have updated idx and
                // out_resolved_options
            } else if (parse_short(&st, out_resolved_options, &local_short_errors,
                                   out_suggestion)) {
                // parse_short succeeded.
            } else {
                /* Unparseable argument.
                 Say the user enters -Dfoo. This may be an unknown long option, or a
                 short option
                 with a value. If there is a short option -D, then it is more likely
                 that the error
                 from the short option parsing is what we want. So ensure the short
                 erorrs appear at
                 the front of the list. */
                if (out_errors) {
                    out_errors->insert(out_errors->begin(), local_long_errors.begin(),
                                       local_long_errors.end());
                    out_errors->insert(out_errors->begin(), local_short_errors.begin(),
                                       local_short_errors.end());
                }
                st.idx += 1;
            }
        } else {
            // Positional argument
            // Note this includes just single-dash arguments, which are often a
            // stand-in for stdin
            out_positionals->emplace_back(st.idx);
            st.idx += 1;
        }
    }
}

#pragma mark -
#pragma mark Matching
#pragma mark -

// The result of parsing argv
typedef std::map<rstring_t, argument_t> option_rmap_t;

// COW helper
// Given a shared_ptr, if the pointer is not the unique owner
// of the object, copy the object and create a new pointer to it
template <typename T>
static T &copy_if_shared(shared_ptr<T> *ptr) {
    if (!ptr->unique()) {
        *ptr = std::make_shared<T>(**ptr);
    }
    return **ptr;
}

// Match state objects
// Match states represent the state of matching argv against our docopt usage tree
// THese are passed around and may be copied, etc.
// Matching is very performance sensitive, so this uses a few optimization tricks
// Implicit copying and moving (via constructors, std::move, etc.) is not allowed
// Instead use copy() and move()
// Also, since states need to diverge but often share structure, we use shared_ptr
// extensively, and copy-on-write
struct match_state_t {
    friend struct match_context_t;

   private:
    // Map from option names to arguments
    shared_ptr<option_rmap_t> argument_values_ref;

    // Bitset of options we've consumed
    shared_ptr<std::vector<bool>> consumed_options_ref;

    // Copying must be explicit via copy()
    // Or use move()
    match_state_t(const match_state_t &) = default;
    match_state_t &operator=(const match_state_t &) = delete;

   public:
    // Next positional to dequeue
    size_t next_positional_index;

    // Suggestions generated for this state
    std::set<rstring_t> suggested_next_arguments;

    // Whether this match has fully consumed all positionals and options
    bool fully_consumed;

    const option_rmap_t &argument_values() const {
        return *argument_values_ref;
    }

    option_rmap_t &mut_argument_values() {
        return copy_if_shared(&argument_values_ref);
    }

    const std::vector<bool> &consumed_options() const {
        return *consumed_options_ref;
    }

    std::vector<bool> &mut_consumed_options() {
        return copy_if_shared(&consumed_options_ref);
    }

    match_state_t(size_t option_count)
        : argument_values_ref(std::make_shared<option_rmap_t>()),
          consumed_options_ref(std::make_shared<std::vector<bool>>(option_count, false)),
          next_positional_index(0),
          fully_consumed(false) {}

    // Default move assignment and constructors
    match_state_t &operator=(match_state_t &&) = default;
    match_state_t(match_state_t &&) = default;

    // Explicit copying only
    match_state_t copy() const {
        return *this;
    }

    // Convenience over std::move()
    match_state_t &&move() {
        return std::move(*this);
    }

    // Returns the "progress" of a state. This is a sum of the number of
    // positionals and
    // arguments consumed, plus the number of suggestions. This is not directly
    // comparable
    // (two states may have identical progress values but be different) but it
    // does capture
    // the relationship between a state and another state derived from it, i.e. if
    // the
    // progress does not change then the child state is identical to its parent.
    size_t progress() const {
        // Add in positionals
        size_t result = this->next_positional_index;

        // Add in arguments by counting set bits in the bitmap
        result +=
            std::accumulate(this->consumed_options().begin(), this->consumed_options().end(), 0);

        // Add in number of suggestions
        result += suggested_next_arguments.size();

        return result;
    }
};

typedef std::vector<match_state_t> match_state_list_t;

struct match_context_t {
   private:
    // Returns true if the state has consumed all positionals and options
    bool has_consumed_everything(const match_state_t &state) const {
        if (has_more_positionals(state)) {
            // Unconsumed positional
            return false;
        }

        // Now return whether all of our consumed options are true
        const std::vector<bool> &ops = state.consumed_options();
        return std::all_of(ops.begin(), ops.end(), [](bool v) { return v; });
    }

   public:
    const parse_flags_t flags;

    // Note: these are stored references.
    // Match context objects are expected to be transient and stack-allocated.
    const option_list_t &shortcut_options;
    const positional_argument_list_t &positionals;
    const resolved_option_list_t &resolved_options;
    const string_list_t &argv;

    bool has_more_positionals(const match_state_t &state) const {
        assert(state.next_positional_index <= this->positionals.size());
        return state.next_positional_index < this->positionals.size();
    }

    // Returns the indexes in argv of the arguments that were unused
    index_list_t unused_arguments(const match_state_t *state) const {
        /* To find the unused arguments, we walk over the used arguments and take
         what's left
         Arguments may be unused for any of three reasons:
         1. It is an unconsumed positional
         2. It is an option that we found in the tree, but was not matched during
         tree descent
         3. It is an option that was not found in the tree at all
         */

        // Make a vector the same size as argv.
        // As we walk over positionals and options, we will mark the corresponding
        // index as used.
        // At the end, the unset bits are the unused arguments
        std::vector<bool> used_indexes(this->argv.size(), false);

        // Walk over used positionals. next_positional_index is the first unused
        // one.
        for (size_t i = 0; i < state->next_positional_index; i++) {
            used_indexes.at(this->positionals.at(i).idx_in_argv) = true;
        }

        // Walk over options matched during tree descent.
        // We should have one bit per option
        assert(state->consumed_options().size() == this->resolved_options.size());
        for (size_t i = 0; i < state->consumed_options().size(); i++) {
            if (state->consumed_options().at(i)) {
                // This option was used. The name index is definitely used. The value
                // index is also
                // used, if it's not npos (note that it may be the same as the name
                // index)
                const resolved_option_t &opt = this->resolved_options.at(i);
                used_indexes.at(opt.name_idx_in_argv) = true;
                if (opt.value_idx_in_argv != npos) {
                    used_indexes.at(opt.value_idx_in_argv) = true;
                }
            }
        }

        // Walk over options NOT matched during tree descent and clear their bits.
        // An argument may be both matched and unmatched, i.e. if "-vv" is parsed
        // into two short
        // options.
        // In that case, we want to mark it as unmatched.
        for (size_t i = 0; i < state->consumed_options().size(); i++) {
            if (!state->consumed_options().at(i)) {
                const resolved_option_t &opt = this->resolved_options.at(i);
                used_indexes.at(opt.name_idx_in_argv) = false;
            }
        }

        // Don't report the first -- as unused
        for (size_t i = 0; i < this->argv.size(); i++) {
            if (is_double_dash(this->argv.at(i))) {
                used_indexes.at(i) = true;
                break;
            }
        }

        // Extract the unused indexes from the bitmap of used arguments
        index_list_t unused_argv_idxs;
        for (size_t i = 0; i < used_indexes.size(); i++) {
            if (!used_indexes.at(i)) {
                unused_argv_idxs.push_back(i);
            }
        }
        return unused_argv_idxs;
    }

    const positional_argument_t &next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index);
    }

    const positional_argument_t &acquire_next_positional(match_state_t *state) const {
        assert(state->next_positional_index < positionals.size());
        return positionals.at(state->next_positional_index++);
    }

    match_context_t(parse_flags_t f, const option_list_t &shortcut_opts,
                    const positional_argument_list_t &p, const resolved_option_list_t &r,
                    const string_list_t &av)
        : flags(f),
          shortcut_options(shortcut_opts),
          positionals(p),
          resolved_options(r),
          argv(av) {}

    /* If we want to stop a search and this state has consumed everything, stop
     * the search */
    void try_mark_fully_consumed(match_state_t *state) const {
        if ((this->flags & flag_stop_after_consuming_everything) &&
            this->has_consumed_everything(*state)) {
            state->fully_consumed = true;
        }
    }
};

static void match(const vector<usage_t> &usages, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const usage_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const expression_list_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const alternation_list_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const expression_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const simple_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const option_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const fixed_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);
static void match(const variable_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states);

static bool match_options(const option_list_t &options_in_doc, match_state_t *state,
                          const match_context_t *ctx);

// TODO: comment me
template <typename T>
static void match_list(const T &node, match_state_list_t *incoming_state_list, match_context_t *ctx,
                       match_state_list_t *resulting_states, bool require_progress = false) {
    for (size_t i = 0; i < incoming_state_list->size(); i++) {
        match_state_t *state = &incoming_state_list->at(i);
        /* If we require that this makes progress, then get the current progress so
         * we can compare
         */
        size_t init_progress = npos;
        size_t init_size = -1;
        if (require_progress) {
            init_progress = state->progress();
            init_size = resulting_states->size();
        }

        match(node, state->move(), ctx, resulting_states);

        if (require_progress) {
            // Keep only those results that have increased in progress. States after
            // init_size are
            // new.
            size_t idx = resulting_states->size();
            assert(idx >= init_size);
            while (idx-- > init_size) {  // last processed idx will be init_size
                size_t new_progress = resulting_states->at(idx).progress();
                assert(new_progress >= init_progress);  // should never go backwards
                if (new_progress == init_progress) {
                    // No progress was made, toss this state
                    resulting_states->erase(resulting_states->begin() + idx);
                }
            }
        }
    }
}

static void match(const vector<usage_t> &usages, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    // Elide the copy in the last one
    size_t count = usages.size();
    if (count == 0) {
        return;
    }

    bool fully_consumed = false;
    for (size_t i = 0; i + 1 < count && !fully_consumed; i++) {
        match(usages.at(i), state.copy(), ctx, resulting_states);

        if (ctx->flags & flag_stop_after_consuming_everything) {
            for (const match_state_t &ms : *resulting_states) {
                if (ms.fully_consumed) {
                    fully_consumed = true;
                    break;
                }
            }
        }
    }
    if (!fully_consumed) {
        match(usages.at(count - 1), state.move(), ctx, resulting_states);
    }
}

static void match(const usage_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    if (!ctx->has_more_positionals(state)) {
        // todo: error handling
        return;
    }

    if (node.prog_name.empty()) {
        // This is the final terminating usage. It does not match anything.
        return;
    }

    // Program name
    ctx->acquire_next_positional(&state);

    // Match against our contents
    match(node.alternation_list, state.move(), ctx, resulting_states);
}

static void match(const expression_list_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    size_t count = node.expressions.size();
    if (count == 0) {
        // Merely append this state
        resulting_states->push_back(state.move());
    } else if (count == 1) {
        // Just one expression, trivial
        match(node.expressions.at(0), state.move(), ctx, resulting_states);
    } else {
        // First expression
        match_state_list_t intermed_state_list;
        match(node.expressions.at(0), state.move(), ctx, &intermed_state_list);
        // Middle expressions
        for (size_t i = 1; i + 1 < count; i++) {
            match_state_list_t new_states;
            match_list(node.expressions.at(i), &intermed_state_list, ctx, &new_states);
            intermed_state_list.swap(new_states);
        }
        // Last expression
        match_list(node.expressions.at(count - 1), &intermed_state_list, ctx, resulting_states);
    }
}

static void match(const alternation_list_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    size_t count = node.alternations.size();
    if (count == 0) {
        return;
    }
    for (size_t i = 0; i + 1 < count; i++) {
        match(node.alternations.at(i), state.copy(), ctx, resulting_states);
    }
    match(node.alternations.at(count - 1), state.move(), ctx, resulting_states);
}

// Ellipsis helper
// Given a node that has ellipsis, the set of resulting states, and an initial
// prior state count,
// match the node repeatedly until we stop making progress
template <typename Node>
static void repeat_matching(const Node &node, size_t init_prior_state_count, match_context_t *ctx,
                            match_state_list_t *resulting_states) {
    assert(resulting_states->size() >= init_prior_state_count);
    match_state_list_t intermediate_states;
    // Get the number of existing states
    // Keep going until we stop getting new states
    size_t prior_state_count = init_prior_state_count;
    while (prior_state_count < resulting_states->size()) {
        // Get a vector of intermediate states
        intermediate_states.clear();
        intermediate_states.reserve(resulting_states->size() - prior_state_count);
        for (size_t i = prior_state_count; i < resulting_states->size(); i++) {
            intermediate_states.push_back(resulting_states->at(i).copy());
        }
        prior_state_count = resulting_states->size();
        match_list(node, &intermediate_states, ctx, resulting_states, true /* require progress */);
    }
}

static void match(const expression_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    // Check to see if we have ellipsis. If so, we keep going as long as we can.
    bool has_ellipsis = node.opt_ellipsis.present;

    switch (node.production) {
        case 0: {
            /* This is a simple clause which may have ellipsis, like foo...
             If we have ellipsis, we match one time, and then construct a sequence of
             'intermediate
             state lists'.
             An intermediate state represents the result of matching N times. Each time
             we construct
             a new intermediate state, we append (copy) all of its states into the
             result; thus we
             may
             match one time, two times, three times...
             We stop when we get no more matches, which usually happens when we run out
             of
             positionals.
             */
            assert(node.simple_clause.get() != nullptr);
            size_t prior_state_count = resulting_states->size();
            match(*node.simple_clause, state.move(), ctx, resulting_states);
            /* Now we know that all states starting at state_count_before are newly
             * added. If we
             * have ellipsis, go until we stop getting new states. */
            if (has_ellipsis) {
                repeat_matching(*node.simple_clause, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 1: {
            /* This is a parenthesized clause which may have ellipsis, like (foo)...
             Same algorithm as the simple clause above.
             TODO: this may loop forever with states that do not consume any values,
             e.g. ([foo])...
             */
            size_t prior_state_count = resulting_states->size();
            assert(node.alternation_list.get() != nullptr);
            match(*node.alternation_list, state.move(), ctx, resulting_states);
            if (has_ellipsis) {
                repeat_matching(*node.alternation_list, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 2: {
            /* This is a square-bracketed clause which may have ellipsis, like [foo]...
             Same algorithm as the simple clause above, except that we also append the
             initial state
             as a not-taken branch.
             */
            assert(node.alternation_list.get() != nullptr);
            resulting_states->push_back(state.copy());  // append the not-taken-branch
            size_t prior_state_count = resulting_states->size();
            match(*node.alternation_list, std::move(state), ctx, resulting_states);
            if (has_ellipsis) {
                repeat_matching(*node.alternation_list, prior_state_count, ctx, resulting_states);
            }
            break;
        }

        case 3: {
            // This is the [options] clause. It does not have ellipsis.
            assert(node.options_shortcut.present);
            if (!match_options(ctx->shortcut_options, &state, ctx)) {
                // No match, but matches are not required
                if (ctx->flags & flag_generate_suggestions) {
                    for (const option_t &opt : ctx->shortcut_options) {
                        state.suggested_next_arguments.insert(opt.best_name());
                    }
                }
            }
            resulting_states->push_back(state.move());
            break;
        }

        default:
            assert(0 && "unknown production");
    }
}

// Match the options in the options list, updating the state
// This returns true if we match at least one
static bool match_options(const option_list_t &options_in_doc, match_state_t *state,
                          const match_context_t *ctx) {
    bool successful_match = false;
    bool made_suggestion = false;

    // Collect potential suggestions in here. We squelch them if we find that a
    // later matched option
    // has the same corresponding long name; we need to remove those from the
    // suggestions
    option_list_t potential_suggestions;

    for (const option_t &opt_in_doc : options_in_doc) {
        // Find the matching option from the resolved option list (i.e. argv)
        size_t resolved_opt_idx = npos;
        for (size_t i = 0; i < ctx->resolved_options.size(); i++) {
            // Skip ones that have already been consumed
            if (!state->consumed_options().at(i)) {
                // See if the option from argv has the same key range as the option in
                // the document
                if (ctx->resolved_options.at(i).option.has_same_name(opt_in_doc)) {
                    resolved_opt_idx = i;
                    break;
                }
            }
        }

        if (resolved_opt_idx != npos) {
            // We found a matching option in argv. Set it in the argument_values for
            // this state and
            // mark its index as used
            // We have two things to set:
            //  - The option name, like -foo
            //  - The option's argument value (if any)
            const resolved_option_t &resolved_opt = ctx->resolved_options.at(resolved_opt_idx);
            const rstring_t &name = opt_in_doc.best_name();

            // Update the option value, creating it if necessary
            state->mut_argument_values()[name].count += 1;

            // Update the option argument vlaue
            if (opt_in_doc.has_value() && resolved_opt.value_idx_in_argv != npos) {
                const rstring_t &variable_name = opt_in_doc.value;

                const rstring_t &value = resolved_opt.value_in_arg;
                state->mut_argument_values()[variable_name].values.push_back(value.std_string());
            }

            successful_match = true;
            state->mut_consumed_options().at(resolved_opt_idx) = true;
        } else {
            // This was an option that was not specified in argv
            // It can be a suggestion
            if (ctx->flags & flag_generate_suggestions) {
                potential_suggestions.push_back(opt_in_doc);
            }
        }
    }

    // Now go through and handle potential suggestions
    if (ctx->flags & flag_generate_suggestions) {
        for (const option_t &suggestion : potential_suggestions) {
            size_t type_idx = option_t::NAME_TYPE_COUNT;
            while (type_idx--) {
                option_t::name_type_t type = static_cast<option_t::name_type_t>(type_idx);
                if (suggestion.has_type(type)) {
                    state->suggested_next_arguments.insert(suggestion.names[type]);
                }
            }
            made_suggestion = true;
        }
    }

    bool matched_something = successful_match || made_suggestion;
    if (matched_something) {
        ctx->try_mark_fully_consumed(state);
    }
    return matched_something;
}

static void match(const simple_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    if (node.option.get()) {
        match(*node.option, state.move(), ctx, resulting_states);
    } else if (node.fixed.get()) {
        match(*node.fixed, state.move(), ctx, resulting_states);
    } else if (node.variable.get()) {
        match(*node.variable, state.move(), ctx, resulting_states);
    } else {
        assert(0 && "Bug in docopt parser: No children of simple_clause.");
    }
}

static void match(const option_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    // Matching an option like --foo
    const option_list_t options_in_doc(1, node.option);
    bool matched = match_options(options_in_doc, &state, ctx);
    if (matched || (ctx->flags & flag_match_allow_incomplete)) {
        resulting_states->push_back(state.move());
    }
}

static void match(const fixed_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    // Fixed argument
    // Compare the next positional to this static argument
    if (ctx->has_more_positionals(state)) {
        const positional_argument_t &positional = ctx->next_positional(&state);
        rstring_t name = rstring_t(ctx->argv.at(positional.idx_in_argv));
        if (node.word == name) {
            // The static argument matches
            state.mut_argument_values()[name].count += 1;
            ctx->acquire_next_positional(&state);
            ctx->try_mark_fully_consumed(&state);
            resulting_states->push_back(state.move());
        }
    } else {
        // No more positionals. Maybe suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state.suggested_next_arguments.insert(node.word);
        }
        // Append the state if we are allowing incomplete
        if (ctx->flags & flag_match_allow_incomplete) {
            resulting_states->push_back(state.move());
        }
    }
}

static void match(const variable_clause_t &node, match_state_t state, match_context_t *ctx,
                  match_state_list_t *resulting_states) {
    // Variable argument
    const rstring_t &name = node.word;
    if (ctx->has_more_positionals(state)) {
        // Note we retain the brackets <> in the variable name
        argument_t *arg = &state.mut_argument_values()[name];
        const positional_argument_t &positional = ctx->acquire_next_positional(&state);
        const string_t &positional_value = ctx->argv.at(positional.idx_in_argv);
        arg->values.push_back(positional_value);
        ctx->try_mark_fully_consumed(&state);
        resulting_states->push_back(state.move());
    } else {
        // No more positionals. Suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state.suggested_next_arguments.insert(name);
        }
        if (ctx->flags & flag_match_allow_incomplete) {
            resulting_states->push_back(state.move());
        }
    }
}

/* Wrapper class that takes either a string or wstring as string_t */
class docopt_impl {
#pragma mark -
#pragma mark Scanning
#pragma mark -

    /* Constructor takes the source in either narrow or wide form. */
   public:
    explicit docopt_impl(string_t s) : usage_storage(std::move(s)) {}
    explicit docopt_impl(std::vector<annotated_option_t> opts)
        : annotated_options(std::move(opts)) {}

    // No copying or moving
    // Moving is supportable but we have no reason to yet
    docopt_impl(const docopt_impl &) = delete;
    docopt_impl &operator=(const docopt_impl &) = delete;
    docopt_impl(docopt_impl &&) = delete;
    docopt_impl &operator=(docopt_impl &&) = delete;

#pragma mark -
#pragma mark Instance Variables
#pragma mark -

    /* The usage parse tree. */
    usage_list_t usages;

    /* When initialized via a usage spec (as a string), we acquire the string. Our
     * rstring_t point
     * into this. */
    const string_t usage_storage;

    /* List of direct options. Some of our rstrings point to strings inside of
     * these. */
    const std::vector<annotated_option_t> annotated_options;

    /* The list of options parsed from the "Options:" section. Referred to as
     * "shortcut options"
     * because the "[options]" directive can be used as a shortcut to reference
     * them. */
    option_list_t shortcut_options;

    /* The list of options parsed from the "Options:" section and "Usage:"
     * sections.  */
    option_list_t all_options;

    /* Map from variable/option names to their metadata */
    metadata_map_t names_to_metadata;

   public:
    /* Populate ourselves from our usage storage. */
    void populate_from_usage(error_list_t *out_errors) {
        // Distinguish between normal (docopt) and exposition (e.g. description). */
        enum mode_t { mode_normal, mode_exposition } mode = mode_normal;

        // We need to parse the usage spec ranges after all of the Options
        // This is because we need the options to disambiguate some usages
        rstring_list_t usage_specs;

        rstring_t line;
        while (get_next_line(this->usage_storage, &line)) {
            /* There are a couple of possibilitise for each line:

             1. It may have a header like "Usage:". If so, we want to strip that
             header, and
             optionally
             use it to determine the mode.
             2. It may be a usage spec. We can tell because the first word is plain
             text.
             3. It may be an option spec. We can tell because the first character is a
             dash.
             4. It may be a variable spec. We can tell because the first character is
             a <.
             5. It may be just whitespace or empty, and is ignored.

             Also note that a (nonempty) line indented more than the previous line is
             considered a
             continuation of that line.
             */
            rstring_t trimmed_line = line.trim_whitespace();

            const rstring_t header = find_header(trimmed_line);
            if (!header.empty()) {
                // It's a header
                // Set mode based on header, and remove header from line
                // The headers we know about are Usage, Synopsis, Options, and Arguments
                // (case
                // insensitive)
                // Everything else is considered exposition
                const char *const keywords[] = {"Usage", "Synopsis", "Options", "Arguments"};
                size_t keyword_count = sizeof keywords / sizeof *keywords;
                bool found_keyword = false;
                for (size_t i = 0; i < keyword_count && !found_keyword; i++) {
                    found_keyword = header.find_case_insensitive(keywords[i]) != rstring_t::npos;
                }
                mode = found_keyword ? mode_normal : mode_exposition;

                // Remove the header range from the trimmed line
                assert(header.length() <= trimmed_line.length());
                trimmed_line = trimmed_line.substr_from(header.length()).trim_whitespace();
            }

            // Skip exposition or empty lines
            if (mode == mode_exposition || trimmed_line.empty()) {
                continue;
            }

            /*
              Compute the indent. Note that the header is considered part of the
              indent, so that:

              Usage: foo
              bar

              Here 'foo' is indented more than 'bar'.
             */
            const size_t line_indent = compute_indent(line, trimmed_line);

            // Determine the "line group." That is, this line plus all subsequent
            // nonempty lines
            // that are indented more than this line.
            rstring_t line_group = trimmed_line;
            rstring_t all_consumed_lines = line;
            rstring_t next_line = line;
            while (get_next_line(this->usage_storage, &next_line)) {
                rstring_t trimmed_next_line = next_line.trim_whitespace();
                size_t next_line_indent = compute_indent(next_line, trimmed_next_line);
                if (trimmed_next_line.empty() || next_line_indent <= line_indent) {
                    break;
                }
                line_group = line_group.merge(next_line);
                all_consumed_lines = all_consumed_lines.merge(next_line);
            }

            rstring_t::char_t first_char = line_group[0];
            if (first_char == '-') {
                // It's an option spec
                this->shortcut_options.push_back(
                    parse_one_option_spec(line_group, &this->names_to_metadata, out_errors));

            } else if (first_char == '<') {
                // It's a variable command spec
                rstring_t variable_name, variable_command;
                if (parse_one_variable_command_spec(line_group, &variable_name, &variable_command,
                                                    out_errors)) {
                    base_metadata_t<rstring_t> *md = &this->names_to_metadata[variable_name];
                    if (!md->command.empty()) {
                        append_docopt_error(out_errors, line_group,
                                            error_one_variable_multiple_commands,
                                            "Duplicate command for variable");
                    }
                    md->command = variable_command;
                }
            } else if (isalnum(first_char) || first_char == '_') {
                // It's a usage spec. We will come back to this.
                usage_specs.push_back(line_group);

            } else {
                // It's an error
                append_docopt_error(out_errors, trimmed_line, error_unknown_leader,
                                    "Lines must start with a normal character, "
                                    "less-than sign, or dash.");
                break;
            }

            // Note the line range we consumed, for the next iteration of the loop
            line = all_consumed_lines;
        }

        // Ensure our shortcut options don't have duplicates
        uniqueize_options(&this->shortcut_options, true /* error on duplicates */, out_errors);

        // Now parse our usage_spec_ranges
        this->usages.reserve(usage_specs.size());
        for (const rstring_t &usage_spec : usage_specs) {
            usage_t usage = parse_one_usage(usage_spec, this->shortcut_options, out_errors);
            this->usages.push_back(std::move(usage));
        }
    }

    // Populate ourselves from our annotated options
    void populate_from_annotated_options() {
        // We're going to construct a list of variable arguments, that are not
        // associated with an option
        // Populate impl->shortcut_options and free_variables.
        rstring_list_t free_variables;
        for (const annotated_option_t &dopt : this->annotated_options) {
            const rstring_t option_name(dopt.option);
            const rstring_t value_name(dopt.value_name);
            
            if (dopt.type == annotated_option_t::value_only) {
                // No option, just a free variable
                assert(! dopt.value_name.empty());
                free_variables.push_back(value_name);
            } else {
                // Create an option
                assert(! dopt.option.empty());
                auto name_type = static_cast<option_t::name_type_t>(dopt.type - 1);
                option_t option(name_type, option_name, value_name);
                if (dopt.value_is_optional) {
                    option.value_is_optional = true;
                    option.separator = option_t::sep_equals;
                }
                this->shortcut_options.push_back(option);
            }

            // Save metadata
            base_metadata_t<rstring_t> md;
            md.command = rstring_t(dopt.metadata.command);
            md.condition = rstring_t(dopt.metadata.condition);
            md.description = rstring_t(dopt.metadata.description);
            md.tag = dopt.metadata.tag;

            if (!option_name.empty()) {
                this->names_to_metadata[option_name] = md;
            }
            if (!value_name.empty()) {
                this->names_to_metadata[value_name] = md;
            }
        }

        bool has_option = !this->shortcut_options.empty();

        // Build a usage
        this->usages.push_back(usage_t::build_from_variables(free_variables, has_option));
    }

    /* Given an option map (using rstring), convert it to an option map using the
     * given
     * std::basic_string type. */
    argument_parser_t::argument_map_t finalize_option_map(const option_rmap_t &map) const {
        argument_parser_t::argument_map_t result;

        // Helper lambda to ensure our result map has a value for a given name
        // We merely invoke operator[]; this will do the insertion with the
        // default (empty) value if necessary. Re-use storage to avoid allocations.
        string_t name_storage;
        auto ensure_argument = [&](const rstring_t &name) -> argument_t & {
            name.copy_to(&name_storage);
            return result[name_storage];
        };

        // Turn our string_ts into std::strings
        for (const auto &kv : map) {
            result.insert({kv.first.std_string(), kv.second});
        }

        // Apply default values for any variables
        for (const option_t &opt : all_options) {
            if (opt.has_value() && !opt.default_value.empty()) {
                argument_t &var_arg = ensure_argument(opt.value);
                if (var_arg.values.empty()) {
                    var_arg.values.push_back(opt.default_value.std_string());
                }
            }
        }

        return result;
    }

    /* Matches argv */
    void match_argv(const string_list_t &argv, parse_flags_t flags,
                    const positional_argument_list_t &positionals,
                    const resolved_option_list_t &resolved_options, option_rmap_t *out_option_map,
                    index_list_t *out_unused_arguments) const {
        /* Set flag_stop_after_consuming_everything. This allows us to early-out. */
        match_context_t ctx(flags | flag_stop_after_consuming_everything, this->shortcut_options,
                            positionals, resolved_options, argv);

        match_state_list_t result;
        match(this->usages, match_state_t(resolved_options.size()), &ctx, &result);

        // Illustration of some logging to help debug matching
        const bool log_stuff = false;
        if (log_stuff) {
            errstream() << "Matched " << result.size() << " way(s)\n";
            for (size_t i = 0; i < result.size(); i++) {
                const match_state_t &state = result.at(i);
                bool is_incomplete = !ctx.unused_arguments(&state).empty();
                errstream() << "Result " << i << (is_incomplete ? " (INCOMPLETE)" : "") << ":\n";
                for (const auto &kv : state.argument_values()) {
                    const rstring_t &name = kv.first;
                    const argument_t &arg = kv.second;
                    errstream() << "\t" << name.std_string() << ": ";
                    for (size_t j = 0; j < arg.values.size(); j++) {
                        if (j > 0) {
                            errstream() << ", ";
                        }
                        errstream() << arg.values.at(j);
                    }
                    errstream() << '\n';
                }
            }
        }

        // Determine the index of the one with the fewest unused arguments
        size_t best_state_idx = npos;
        index_list_t best_unused_args;
        for (size_t i = 0; i < result.size(); i++) {
            const match_state_t &state = result.at(i);
            index_list_t unused_args = ctx.unused_arguments(&state);
            size_t unused_arg_count = unused_args.size();
            if (i == 0 || unused_arg_count < best_unused_args.size()) {
                best_state_idx = i;
                best_unused_args = std::move(unused_args);
                // If we got zero, we're done
                if (unused_arg_count == 0) {
                    break;
                }
            }
        }

        // Now return the winning state and its unused arguments
        if (best_state_idx != npos) {
            // We got a best state
            if (out_unused_arguments != nullptr) {
                *out_unused_arguments = std::move(best_unused_args);
            }
            if (out_option_map != nullptr) {
                *out_option_map = std::move(result.at(best_state_idx).mut_argument_values());
            }
        } else {
            // No states. Every argument is unused.
            if (out_unused_arguments != nullptr) {
                out_unused_arguments->clear();
                for (size_t i = 0; i < argv.size(); i++) {
                    out_unused_arguments->push_back(i);
                }
            }
            if (out_option_map != nullptr) {
                out_option_map->clear();
            }
        }
    }

    /* Parses the docopt, etc. Returns true on success, false on error */
    bool preflight(error_list_t *out_errors) {
        /* If we have no usage, apply the default one */
        if (this->usages.empty()) {
            this->usages.push_back(usage_t::make_default());
        }

        // Extract options and variables from the usage sections
        option_list_t usage_options = collect_options(this->usages);

        // Combine these into a single list
        this->all_options.reserve(usage_options.size() + this->shortcut_options.size());
        this->all_options.insert(this->all_options.end(), usage_options.begin(),
                                 usage_options.end());
        this->all_options.insert(this->all_options.end(), this->shortcut_options.begin(),
                                 this->shortcut_options.end());
        uniqueize_options(&this->all_options, false /* do not error on duplicates */, out_errors);

        /* Hackish. Consider the following usage:
         usage: prog [options] [-a]
         options: -a

         invoked as: prog -a -a

         Naively we would expect options to match the first -a arg, and the -a from
         usage to match
         the second. But we don't. Instead, if an option appears explicitly in a
         usage pattern, we
         excise it from the shortcuts.

         What Python docopt does is assert, if an option appears anywhere in any
         usage, it may not
         be matched by [options]. This seems reasonable, because it means that that
         option has more
         particular use cases. So remove all items from shortcut_options() that
         appear in
         usage_options.

         TODO: this currently only removes the matched variant. For example, prog -a
         --alpha would
         still be allowed.
         */

        auto opt_is_in_usage = [&](const option_t &shortcut_opt) {
            for (const option_t &usage_opt : usage_options) {
                if (shortcut_opt.has_same_name(usage_opt)) {
                    return true;
                }
            }
            return false;
        };

        this->shortcut_options.erase(std::remove_if(this->shortcut_options.begin(),
                                                    this->shortcut_options.end(), opt_is_in_usage),
                                     this->shortcut_options.end());

        // Example of how to dump
        if ((0)) {
            string_t dumped;
            for (size_t i = 0; i < this->usages.size(); i++) {
                dumped += node_dumper_t::dump_tree(this->usages.at(i));
            }
            errstream() << dumped << "\n";
        }

        /* Successfully preflighted */
        return true;
    }

    void best_assignment_for_argv(const string_list_t &argv, parse_flags_t flags,
                                  error_list_t *out_errors, index_list_t *out_unused_arguments,
                                  option_rmap_t *out_option_map) const {
        positional_argument_list_t positionals;
        resolved_option_list_t resolved_options;

        // Extract positionals and arguments from argv
        separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals,
                                                   &resolved_options, out_errors);

        // Produce an option map
        this->match_argv(argv, flags, positionals, resolved_options, out_option_map,
                         out_unused_arguments);
    }

    rstring_list_t suggest_next_argument(const string_list_t &argv, parse_flags_t flags) const {
        /* Set internal flags to generate suggestions */
        flags |= flag_generate_suggestions;

        positional_argument_list_t positionals;
        resolved_option_list_t resolved_options;
        rstring_t suggestion;
        separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals,
                                                   &resolved_options, nullptr /* errors */,
                                                   &suggestion);

        // If we got a suggestion, it means that the last argument was of the form
        // --foo, where --foo wants a value. That's all we care about.
        if (!suggestion.empty()) {
            return rstring_list_t(1, suggestion);
        }

        match_context_t ctx(flags, shortcut_options, positionals, resolved_options, argv);
        match_state_list_t states;
        match(this->usages, match_state_t(resolved_options.size()), &ctx, &states);

        // Find the state(s) with the fewest unused arguments,
        // and then insert all of their suggestions into a list
        rstring_list_t all_suggestions;
        size_t best_unused_arg_count = (size_t)-1;
        for (const match_state_t &state : states) {
            size_t count = ctx.unused_arguments(&state).size();
            if (count < best_unused_arg_count) {
                best_unused_arg_count = count;
            }
        }
        for (const match_state_t &state : states) {
            if (ctx.unused_arguments(&state).size() == best_unused_arg_count) {
                all_suggestions.insert(all_suggestions.end(),
                                       state.suggested_next_arguments.begin(),
                                       state.suggested_next_arguments.end());
            }
        }
        // Eliminate duplicates
        std::sort(all_suggestions.begin(), all_suggestions.end());
        all_suggestions.erase(std::unique(all_suggestions.begin(), all_suggestions.end()),
                              all_suggestions.end());
        return all_suggestions;
    }

    base_metadata_t<rstring_t> metadata_for_name(const rstring_t &name) const {
        base_metadata_t<rstring_t> result;
        metadata_map_t::const_iterator where = this->names_to_metadata.find(name);
        if (where != this->names_to_metadata.end()) {
            result = where->second;
        }
        return result;
    }

    std::vector<string_t> get_command_names() const {
        // Get the command names.
        // We store a set of seen names so we only return each name once,
        // but in the order matching their appearance in the usage spec.
        std::vector<string_t> result;
        std::set<rstring_t> seen;
        for (const usage_t &usage : this->usages) {
            const rstring_t &name = usage.prog_name;
            if (!name.empty() && seen.insert(name).second) {
                result.push_back(name.std_string());
            }
        }
        return result;
    }
};  // docopt_impl

std::vector<argument_status_t> argument_parser_t::validate_arguments(
    const std::vector<string_t> &argv, parse_flags_t flags) const {
    size_t arg_count = argv.size();
    std::vector<argument_status_t> result(arg_count, status_valid);

    index_list_t unused_args;
    impl->best_assignment_for_argv(argv, flags, nullptr /* errors */, &unused_args, nullptr);

    // Unused arguments are all invalid
    for (size_t unused_arg_idx : unused_args) {
        result.at(unused_arg_idx) = status_invalid;
    }
    return result;
}

string_list_t argument_parser_t::suggest_next_argument(const string_list_t &argv,
                                                       parse_flags_t flags) const {
    rstring_list_t suggestions = impl->suggest_next_argument(argv, flags);

    string_list_t result;
    result.reserve(suggestions.size());
    for (const rstring_t &suggestion : suggestions) {
        result.push_back(suggestion.std_string());
    }
    return result;
}

metadata_t argument_parser_t::metadata_for_name(const string_t &var) const {
    const base_metadata_t<rstring_t> md = impl->metadata_for_name(rstring_t(var));
    metadata_t result;
    result.command = md.command.std_string();
    result.condition = md.condition.std_string();
    result.description = md.description.std_string();
    result.tag = md.tag;
    return result;
}

string_list_t argument_parser_t::get_command_names() const {
    return impl->get_command_names();
}

argument_parser_t::argument_map_t argument_parser_t::parse_arguments(
    const string_list_t &argv, parse_flags_t flags, error_list_t *out_errors,
    std::vector<size_t> *out_unused_arguments) const {
    option_rmap_t option_rmap;
    impl->best_assignment_for_argv(argv, flags, out_errors, out_unused_arguments, &option_rmap);
    return impl->finalize_option_map(option_rmap);
}

bool argument_parser_t::set_doc(string_t doc, error_list_t *out_errors) {
    docopt_impl *new_impl = new docopt_impl(std::move(doc));
    new_impl->populate_from_usage(out_errors);
    bool preflighted = new_impl->preflight(out_errors);
    if (preflighted) {
        // install the new impl
        // any existing one will be deleted
        std::swap(this->impl, new_impl);
    }
    delete new_impl;  // may be null
    return preflighted;
}

void argument_parser_t::set_options(std::vector<annotated_option_t> opts) {
    docopt_impl *new_impl = new docopt_impl(std::move(opts));
    new_impl->populate_from_annotated_options();
    bool preflighted = new_impl->preflight(nullptr);
    if (preflighted) {
        // install the new impl
        // any existing one will be deleted
        std::swap(this->impl, new_impl);
    }
    delete new_impl;  // may be null
}

/* Constructors */

argument_parser_t::argument_parser_t() : impl(nullptr) {}

argument_parser_t::argument_parser_t(const string_t &doc, error_list_t *out_errors)
    : impl(nullptr) {
    this->set_doc(doc, out_errors);
}

argument_parser_t::argument_parser_t(argument_parser_t &&rhs) {
    this->impl = rhs.impl;
    rhs.impl = nullptr;
}

argument_parser_t &argument_parser_t::operator=(argument_parser_t &&rhs) {
    std::swap(this->impl, rhs.impl);
    return *this;
}

/* Destructor */

argument_parser_t::~argument_parser_t() {
    delete impl;  // may be null
}

// close the namespace
CLOSE_DOCOPT_IMPL
