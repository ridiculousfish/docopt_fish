#include "docopt_fish.h"
#include "docopt_fish_types.h"
#include "docopt_fish_grammar.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <set>

#if defined(_LIBCPP_VERSION) || __cplusplus > 199711L
// C++11 or libc++ (which is a C++11-only library, but the memory header works OK in C++03)
#include <memory>
using std::shared_ptr;
#else
// C++03 or libstdc++
#include <tr1/memory>
using std::tr1::shared_ptr;
#endif


namespace docopt_fish
OPEN_DOCOPT_IMPL

static const size_t npos = (size_t)(-1);

typedef std::vector<rstring_t> rstring_list_t;
typedef std::vector<error_t> error_list_t;
typedef std::vector<size_t> index_list_t;

/* Class representing a map from variable names to commands */
typedef std::map<rstring_t, rstring_t> variable_command_map_t;

// This represents an error in argv, i.e. the docopt description was OK but a parameter contained an error
static void append_argv_error(error_list_t *errors, size_t arg_idx, int code, const char *txt, size_t pos_in_arg = 0) {
    append_error(errors, pos_in_arg, code, txt, arg_idx);
}

// This represents an error in the docopt specification itself
// The token is a substring of the docopt spec, and its location is used to determine the error position
static void append_docopt_error(error_list_t *errors, const rstring_t &token, int code, const char *txt) {
    append_error(errors, token.start(), code, txt, -1);
}

/* Parsing helpers */
template<char T>
bool it_equals(rstring_t::char_t c) { return c == T; }

bool char_is_valid_in_parameter(rstring_t::char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

bool char_is_valid_in_bracketed_word(rstring_t::char_t c) {
    const char *invalid = "|()[]>\t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

bool char_is_space(rstring_t::char_t c) {
    return c == ' ';
}

/* Given an inout string, parse out an option and return it. Update the string to reflect the number of characters used. */
bool option_t::parse_from_string(rstring_t *remaining, option_t *result, error_list_t *errors) {
    assert(! remaining->empty());
    
    bool errored = false;
    
    // Count how many leading dashes
    rstring_t leading_dashes = remaining->scan_while<it_equals<'-'> >();
    const size_t dash_count = leading_dashes.length();
    assert(dash_count > 0);
    if (dash_count > 2) {
        append_docopt_error(errors, leading_dashes, error_excessive_dashes, "Too many dashes");
    }
    
    // Walk over characters valid in a name
    rstring_t name = remaining->scan_while<char_is_valid_in_parameter>();
    
    // Check to see if there's a space
    rstring_t space_separator = remaining->scan_while<char_is_space>();
    
    // Check to see if there's an = sign
    const rstring_t equals = remaining->scan_while<it_equals<'='> >();
    if (equals.length() > 1) {
        append_docopt_error(errors, equals, error_excessive_equal_signs, "Too many equal signs");
        errored = true;
    }
    
    // Try to scan a variable
    // TODO: If we have a naked equals sign (foo = ) generate an error
    remaining->scan_while<char_is_space>();
    
    rstring_t variable;
    rstring_t open_sign = remaining->scan_1_char('<');
    if (! open_sign.empty()) {
        rstring_t variable_name = remaining->scan_while<char_is_valid_in_bracketed_word>();
        rstring_t close_sign = remaining->scan_1_char('>');
        if (variable_name.empty()) {
            append_docopt_error(errors, variable_name, error_invalid_variable_name, "Missing variable name");
            errored = true;
        } else if (close_sign.empty()) {
            append_docopt_error(errors, open_sign, error_invalid_variable_name, "Missing '>' to match this '<'");
            errored = true;
        } else {
            variable = open_sign.merge(variable_name).merge(close_sign);
        }
        
        // Check to see what the next character is. If it's not whitespace or the end of the string, generate an error.
        if (! close_sign.empty() && ! remaining->empty() && char_is_valid_in_parameter(remaining->at(0))) {
            append_docopt_error(errors, *remaining, error_invalid_variable_name, "Extra stuff after closing '>'");
            errored = true;
        }
    }
    
    // Report an error for cases like --foo=
    if (variable.empty() && ! equals.empty()) {
        append_docopt_error(errors, equals, error_invalid_variable_name, "Missing variable for this assignment");
        errored = true;
    }
    
    // Determine the separator type
    // If we got an equals range, it's something like 'foo = <bar>' or 'foo=<bar>'. The separator is equals and the space is ignored.
    // Otherwise, the space matters: 'foo <bar>' is space-separated, and 'foo<bar>' has no separator
    // Hackish: store sep_space for options without a separator
    option_t::separator_t separator;
    if (variable.empty()) {
        separator = option_t::sep_space;
    } else if (! equals.empty()) {
        separator = option_t::sep_equals;
    } else if (! space_separator.empty()) {
        separator = option_t::sep_space;
    } else {
        separator = option_t::sep_none;
    }
    
    // Generate an error on long options with no separators (--foo<bar>). Only short options support these.
    if (separator == option_t::sep_none && (dash_count > 1 || name.length() > 1)) {
        append_docopt_error(errors, name, error_bad_option_separator, "Long options must use a space or equals separator");
        errored = true;
    }
    
    // Generate errors for missing name
    if (name.empty()) {
        append_docopt_error(errors, name, error_invalid_option_name, "Missing option name");
        errored = true;
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
    
    // Create and return the option
    if (! errored) {
        *result = option_t(type, leading_dashes.merge(name), variable, separator);
    }
    return ! errored;
}

option_t option_t::parse_from_argument(const rstring_t &str, option_t::name_type_t type) {
    assert(! str.empty());
    assert(str.at(0) == '-');
    
    rstring_t remaining(str);
    
    // Get the name part
    const rstring_t dashes = remaining.scan_while<it_equals<'-'> >();
    const rstring_t name = remaining.scan_while<char_is_valid_in_parameter>();
    
    // Check to see if there's an = sign
    const rstring_t equals = remaining.scan_1_char('=');
    
    // If we got an equals sign, the rest is the value
    // It can have any character at all, since it's coming from the argument, not from the usage spec
    rstring_t value;
    if (! equals.empty()) {
        value = remaining;
    }
    
    // Return the option
    return option_t(type, dashes.merge(name), value, equals.empty() ? option_t::sep_space : option_t::sep_equals);
}

/* Helper class for pretty-printing */
class node_dumper_t : public node_visitor_t<node_dumper_t> {
    unsigned int depth;
    
    std::vector<std::string> lines;
    
    node_dumper_t() : depth(0) {}
    
public:
    template<typename NODE_TYPE>
    void accept(const NODE_TYPE& node) {
        std::string result(2 * depth, ' ');
        result.append(node.name());
        lines.push_back(result);
    }
    
    /* Override of visit() to bump the depth */
    template<typename NODE_TYPE>
    void visit(const NODE_TYPE &t)
    {
        depth += 1;
        node_visitor_t<node_dumper_t>::visit(t);
        depth -= 1;
    }
    
    void accept(const rstring_t &t1) {
        if (! t1.empty()) {
            std::string result(2 * depth, ' ');
            
            std::string tmp;
            t1.copy_to(&tmp);
            result += "'" + tmp + "'";
            
            char buff[32];
            if (t1.length() == 1) {
                snprintf(buff, sizeof buff, "{%lu}", t1.start());
            } else {
                snprintf(buff, sizeof buff, "{%lu-%lu}", t1.start(), t1.length());
            }
            result.append(buff);
            lines.push_back(result);
        }
    }
    
    template<typename NODE_TYPE>
    static std::string dump_tree(const NODE_TYPE &node) {
        node_dumper_t dumper;
        dumper.begin(node);
        std::string result;
        for (size_t i=0; i < dumper.lines.size(); i++) {
            result.append(dumper.lines.at(i));
            result.push_back('\n');
        }
        return result;
    }
};

/* Helper class for collecting clauses from a tree */
struct clause_collector_t : public node_visitor_t<clause_collector_t> {
    option_list_t options;
    rstring_list_t fixeds;
    rstring_list_t variables;
    
    // The requested types we capture
    void accept(const option_clause_t& node) {
        options.push_back(node.option);
    }
    
    void accept(const fixed_clause_t& node) {
        fixeds.push_back(node.word);
    }
    
    void accept(const variable_clause_t& node) {
        variables.push_back(node.word);
    }
    
    // Other types we ignore
    template<typename IGNORED_TYPE>
    void accept(const IGNORED_TYPE& t UNUSED) {}
};

/* Helper to efficiently iterate over lines of a string 'base'. inout_line should be initially empty. On return, it will contain the line, with its end pointing just after the trailing newline, or possibly at the end. Returns true if a line was returned, false if we reached the end. */
static bool get_next_line(const rstring_t &base, rstring_t *inout_line) {
    assert(inout_line != NULL);
    if (inout_line->end() == base.end()) {
        // Line exhausted
        return false;
    }
    
    // Start at the end of the last line, or zero if this is the first call
    // Subtract off base.start() to make the line_start relative to base
    size_t line_start = (inout_line->empty() ? 0 : inout_line->end() - base.start());
    rstring_t remainder = base.substr_from(line_start);
    size_t newline = remainder.find("\n");
    if (newline == rstring_t::npos) {
        // Take everything
        *inout_line = remainder;
    } else {
        // Take through the newline
        *inout_line = remainder.substr(0, newline+1);
    }
    // Empty lines are impossible
    assert(! inout_line->empty());
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
    
    // The range within that argument where the value was found. This will be the entire string if the argument is separate (--foo bar) but will be the portion after the equals if not (--foo=bar, -Dfoo)
    rstring_t value_in_arg;
    
    resolved_option_t(const option_t &opt, size_t name_idx, size_t val_idx, const rstring_t &value) : option(opt), name_idx_in_argv(name_idx), value_idx_in_argv(val_idx), value_in_arg(value)
    {}
};
typedef std::vector<resolved_option_t> resolved_option_list_t;

/* List of usages */
typedef std::vector<usage_t> usage_list_t;

/* Collects options, i.e. tokens of the form --foo */
static void collect_options_and_variables(const usage_list_t &usages, option_list_t *out_options, rstring_list_t *out_variables, rstring_list_t *out_static_arguments) {
    clause_collector_t collector;
    for (size_t i=0; i < usages.size(); i++) {
        collector.begin(usages.at(i));
    }
    
    // "Return" the values
    out_options->swap(collector.options);
    out_variables->swap(collector.variables);
    out_static_arguments->swap(collector.fixeds);
}


/* A positional argument */
struct positional_argument_t {
    size_t idx_in_argv;
    
    explicit positional_argument_t(size_t idx) : idx_in_argv(idx)
    {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;

/* Given an option spec, that extends from the initial - to the end of the description, parse out an option. It may have multiple names. */
static option_t parse_one_option_spec(const rstring_t &spec, error_list_t *errors) {
    assert(! spec.empty() && spec[0] == '-');
    const size_t end = spec.length();
    option_t result;
    
    // Look for two spaces. Those separate the description.
    size_t options_end = spec.find("  ");
    if (options_end > end) {
        options_end = end; // no description
    }
    
    // Determine the description range (possibly empty). Trim leading and trailing whitespace
    rstring_t description = spec.substr_from(options_end).trim_whitespace();
    result.description = description;
    
    // Parse out a "default:" value.
    if (! description.empty()) {
        // TODO: handle the case where there's more than one
        const char *default_prefix = "[default:";
        size_t default_prefix_loc = description.find_case_insensitive(default_prefix);
        if (default_prefix_loc != rstring_t::npos) {
            rstring_t default_value = description.substr_from(default_prefix_loc + strlen(default_prefix)).trim_whitespace();
            
            // Find the closing ']'
            size_t default_value_end = default_value.find("]");
            if (default_value_end == rstring_t::npos) {
                append_docopt_error(errors, default_value, error_missing_close_bracket_in_default, "Missing ']' to match opening '['");
            } else {
                result.default_value = default_value.substr(0, default_value_end);
            }
        }
    }
    
    // Parse the options portion
    rstring_t remaining = spec.substr(0, options_end);
    remaining.scan_while<char_is_space>();
    while (! remaining.empty()) {
        if (remaining[0] != '-') {
            append_docopt_error(errors, remaining, error_invalid_option_name, "Not an option");
            break;
        }
        
        option_t opt;
        if (! option_t::parse_from_string(&remaining, &opt, errors)) {
            // Failed to get an option, give up
            break;
        }
        result.merge_from(opt);
        
        // Skip over commas, which separate arguments
        remaining.scan_while<char_is_space>();
        remaining.scan_while<it_equals<','> >();
        remaining.scan_while<char_is_space>();
    }
    
    return result;
}

/* Returns a header in the given string, or an empty string if none. We are considered a header if we contain a colon, and only space / alpha text before it. */
static rstring_t find_header(const rstring_t &src) {
    rstring_t result;
    for (size_t i=0; i < src.length(); i++) {
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

/* Given a variable spec, parse out a condition map */
static variable_command_map_t parse_one_variable_command_spec(const rstring_t &spec, error_list_t *out_errors) {
    // A specification look like this:
    // <pid> stuff
    variable_command_map_t result;
    assert(! spec.empty() && spec[0] == '<');
    const size_t close_bracket = spec.find('>');
    if (close_bracket == rstring_t::npos) {
        append_docopt_error(out_errors, spec, error_missing_close_variable, "No > to balance this <");
    } else {
        assert(close_bracket < spec.length());
        rstring_t key = spec.substr(0, close_bracket+1).trim_whitespace();
        rstring_t value = spec.substr_from(close_bracket+1).trim_whitespace();
        result[key] = value;
    }
    return result;
}


// Given a string 'src' and a whitespace-trimmed substring trimmed_src, compute how much trimmed_src is indented. Tabs are treated as 4 spaces. newlines are unexpected, and treated as one space.
static size_t compute_indent(const rstring_t &src, const rstring_t &trimmed_src) {
    assert(trimmed_src.start() >= src.start() && trimmed_src.end() <= src.end());
    const size_t tabstop = 4;
    // Walk over the prefix of src, up to trimmed_src
    size_t result = 0, length = trimmed_src.start() - src.start();
    for (size_t i=0; i < length; i++) {
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

/* Given a list of options, verify that any duplicate options are in agreement, and remove all but one. TODO: Can we make this not N^2 without heap allocation? */
static void uniqueize_options(option_list_t *options, bool error_on_duplicates, error_list_t *errors) {
    // Maintain an outer cursor. For each option, loop over the remainder, deleting those that share a name
    // Grab the best description as we go
    // We "delete" from the middle of the vector by moving the last element into the slot, and then decrementing the length
    size_t options_count = options->size(); // note this changes as we go
    for (size_t outer = 0; outer < options_count; outer++) {
        option_t *representative = &options->at(outer);

        // Find all options that share a name with this representative
        // Determine which one is best
        // Overwrite them with an empty option, so we skip them next
        for (size_t match_cursor = outer + 1; match_cursor < options_count; match_cursor++) {
            option_t *candidate = &options->at(match_cursor);
            if (! representative->has_same_name(*candidate)) {
                continue;
            }
            
            // Ok, we know that candidate has the same name as the best match
            // Generate an error if we're supposed to
            // TODO: verify agreement in the parameters, etc.
            // Then we copy the description over if it's better, then "erase" the candidate
            // That will cause us to skip over it later
            if (error_on_duplicates) {
                // Generate an error, and then continue on
                append_docopt_error(errors, candidate->best_name(), error_option_duplicated_in_options_section, "Option specified more than once");
            }
            if (candidate->description.length() > representative->description.length()) {
                representative->description = candidate->description;
            }
            
            // "Delete" candidate by overwriting it with the last value, and decrementing the count
            *candidate = options->back();
            options->pop_back();
            options_count -= 1;
            match_cursor -= 1; // have to re-evaluate this value
        }
    }
}

/* Transient stack-allocated data associated with separating argv */
struct argv_separation_state_t {
    const rstring_list_t &argv;
    const option_list_t &options;
    parse_flags_t flags;
    size_t idx;
    bool saw_double_dash;
    
    argv_separation_state_t(const rstring_list_t &argv_, const option_list_t &options_, parse_flags_t flags_) : argv(argv_), options(options_), flags(flags_), idx(0), saw_double_dash(false)
    {}
    
    const rstring_t &arg() const {
        return this->argv.at(this->idx);
    }
    
    bool has_double_dash_at(size_t idx) const {
        return idx < this->argv.size() && this->argv.at(idx).is_double_dash();
    }
};

/* Extracts a long option from the arg at idx, and appends the result to out_result. Updates idx.
 TODO: merge with parse_unseparated_short, etc
 */
static bool parse_long(argv_separation_state_t *st, option_t::name_type_t type, resolved_option_list_t *out_result, error_list_t *out_errors, rstring_t *out_suggestion) {
    const rstring_t &arg = st->arg();
    assert(type == option_t::single_long || type == option_t::double_long);
    assert(arg.has_prefix(type == option_t::single_long ? "-" : "--"));
    
    /* Parse the argument into an 'option'. Note that this option does not appear in the options list because its range reflects the string in the argument. TODO: Need to distinguish between equivalent ways of specifying parameters (--foo=bar and --foo bar) */
    option_t arg_as_option = option_t::parse_from_argument(arg, type);
    assert(arg_as_option.separator != option_t::sep_none);
    
    const rstring_t arg_name = arg_as_option.names[type];
    assert(! arg_name.empty());
    const size_t arg_length = arg_name.length();
    
    /* Get list of matching long options. */
    option_list_t matches;
    for (size_t i=0; i < st->options.size(); i++) {
        const option_t &opt = st->options.at(i);
        if (opt.has_type(type) && opt.names[type] == arg_name) {
            // Should never have separator_none for long options
            assert(opt.separator != option_t::sep_none);
            matches.push_back(opt);
        }
    }
    
    if (matches.empty() && (st->flags & flag_resolve_unambiguous_prefixes)) {
        /* We didn't get any direct matches; look for an unambiguous prefix match */
        option_list_t prefix_matches;
        for (size_t i=0; i < st->options.size(); i++) {
            const option_t &opt = st->options.at(i);
            // Here we confirm that the option's name is longer than the name portion of the argument.
            // If they are equal; we would have had an exact match above; if the option is shorter, then the argument is not a prefix of it.
            // If the option is longer, we then do a substring comparison, up to the number of characters determined by the argument
            if (opt.has_type(type) && opt.names[type].length() > arg_length && arg_name == opt.names[type].substr(0, arg_length)) {
                prefix_matches.push_back(opt);
            }
        }
        if (prefix_matches.size() > 1) {
            // Todo: list exactly the different options that this prefix can correspond to
            append_argv_error(out_errors, st->idx, error_ambiguous_prefix_match, "Ambiguous prefix match");
        } else if (prefix_matches.size() == 1) {
            // We have one unambiguous prefix match. Swap it into the true matches array, which is currently empty.
            matches.swap(prefix_matches);
        } else {
            // Empty, no prefix match at all. Continue on.
        }
    }
    
    /* TODO: Better error reporting */
    /* TODO: can eliminate matches array entirely, just use a single index */
    
    bool success = false;
    size_t match_count = matches.size();
    // Our option de-duplication ensures we should never have more than one match
    assert(match_count <= 1);
    if (match_count < 1) {
        append_argv_error(out_errors, st->idx, error_unknown_option, "Unknown long option");
    } else {
        bool errored = false;
        assert(match_count == 1);
        const option_t &match = matches.at(0);
        
        /* Ensure the option and argument agree on having a value */
        rstring_t value;
        const size_t name_idx = st->idx;
        size_t arg_index = npos;
        if (match.has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar. The range is the value portion; the index is the same as our argument.
                value = arg_as_option.value;
                arg_index = st->idx;
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
                } else if ((st->flags & flag_generate_suggestions) && out_suggestion != NULL) {
                    // We are at the last argument, and we expect a value. Return the value as a suggestion.
                    *out_suggestion = match.value;
                    errored = true;
                } else {
                    append_argv_error(out_errors, st->idx, error_option_has_missing_argument, "Option expects an argument");
                    errored = true;
                }
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            append_argv_error(out_errors, st->idx, error_option_unexpected_argument, "Option does not expect an argument");
            errored = true;
        }
        
        // If we want strict separators, check for separator agreement
        if (! errored && (st->flags & flag_short_options_strict_separators)) {
            if (arg_as_option.separator != match.separator) {
                // TODO: improve this error
                append_argv_error(out_errors, st->idx, error_wrong_separator, "Option expects a different separator");
                errored = true;
            }
        }
        
        if (! errored) {
            out_result->push_back(resolved_option_t(match, name_idx, arg_index, value));
            st->idx += 1;
            success = true;
        }
    }
    return success;
}

// Given a list of short options, try parsing out an unseparated short, i.e. -DNDEBUG. We only look at short options with no separator. TODO: Use out_suggestion
static bool parse_unseparated_short(argv_separation_state_t *st, resolved_option_list_t *out_result, error_list_t *out_errors, rstring_t *out_suggestion UNUSED) {
    const rstring_t arg(st->arg());
    // must not be just a single dash
    assert(arg.length() > 1 && arg.at(0) == '-');
    bool success = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> matches;
    
    // If strict_separators is set, then we require that the option have sep_none
    // If not set, then we don't care if the separators match
    const bool relaxed_separators = ! (st->flags & flag_short_options_strict_separators);
    
    for (size_t i=0; i < st->options.size(); i++) {
        const option_t &opt = st->options.at(i);
        if (opt.has_type(option_t::single_short) && opt.has_value() && (relaxed_separators || opt.separator == option_t::sep_none)) {
            // Candidate short option.
            // This looks something like -DNDEBUG. We want to see if the D matches.
            // Compare the character at offset 1 (to account for the dash) and length 1 (since it's a short option)
            if (opt.names[option_t::single_short].at(1) == arg.at(1)) {
                // Expect to always want a value here
                matches.push_back(opt);
            }
        }
    }
    
    // Our option de-duplication should ensure we should never have more than one match
    assert(matches.size() <= 1);
    if (matches.size() == 1) {
        // Try to extract the value. This is very simple: it starts at index 2 and goes to the end of the arg.
        const option_t &match = matches.at(0);
        if (arg.length() <= 2) {
            append_argv_error(out_errors, st->idx, error_option_has_missing_argument, "Option expects an argument");
        } else {
            // Got one
            size_t name_idx = st->idx;
            size_t value_idx = st->idx;
            rstring_t value = arg.substr_from(2);
            out_result->push_back(resolved_option_t(match, name_idx, value_idx, value));
            st->idx += 1;
            success = true;
        }
    } else {
        // Common case: Match count is 0, so we didn't get any.
        assert(matches.empty());
    }
    return success;
}

// Given a list of short options, parse out an argument
static bool parse_short(argv_separation_state_t *st, resolved_option_list_t *out_result, error_list_t *out_errors, rstring_t *out_suggestion) {
    const rstring_t &arg = st->arg();
    assert(arg.has_prefix("-"));
    assert(arg.length() > 1); // must not be just a single dash
    bool errored = false;
    bool last_option_has_argument = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> options_for_argument;
    
    std::vector<option_t> matches;
    for (size_t idx_in_arg=1; idx_in_arg < arg.length() && ! errored; idx_in_arg++) {
        /* Get list of short options matching this resolved option. */
        const rstring_t::char_t short_char = arg.at(idx_in_arg);
        matches.clear();
        for (size_t i=0; i < st->options.size(); i++) {
            const option_t &opt = st->options.at(i);
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
            append_argv_error(out_errors, st->idx, error_unknown_option, "Unknown short option", idx_in_arg);
            errored = true;
        } else {
            // Just one match, add it to the global array
            options_for_argument.push_back(matches.at(0));
        }
    }
    
    if (! errored) {
        // Now we have all of the short options that this argument represents. No option is allowed to have a value, except possibly last one.
        for (size_t i=0; i < options_for_argument.size(); i++) {
            const option_t &opt = options_for_argument.at(i);
            if (opt.has_value()) {
                if (i + 1 == options_for_argument.size()) {
                    // This is the last option
                    last_option_has_argument = true;
                } else {
                    // This is not the last option
                    // This i+1 is the position in the argument and needs some explanation. Since we have a leading dash and then an argument, which is parsed into short options - one per character, including the dash. Hence we can map from index-in-option to index-in-argument, unless there was an unknown option error above. In that case this will be wrong (but we typically only show the first error anyways).
                    append_argv_error(out_errors, st->idx, error_option_unexpected_argument, "Option may not have a value unless it is the last option", i + 1);
                }
            }
        }
    }
    
    // If we have an argument, determine its index
    rstring_t value_for_last_option;
    const size_t name_idx = st->idx;
    size_t val_idx_for_last_option = npos;
    if (! errored && last_option_has_argument) {
        // We don't support -f=bar style. I don't know of any commands that use this.
        // TODO: support delimiter-free style (gcc -Dmacro=something)
        // Handle possible double-dash
        if (st->has_double_dash_at(st->idx + 1)) {
            st->saw_double_dash = true;
            st->idx += 1;
        }
        
        if (st->idx + 1 < st->argv.size()) {
            val_idx_for_last_option = st->idx + 1;
            value_for_last_option = rstring_t(st->argv.at(st->idx + 1));
        } else if ((st->flags & flag_generate_suggestions) && out_suggestion != NULL) {
            // We are at the last argument, and we expect a value. Return the value as a suggestion.
            const option_t &match = options_for_argument.back();
            *out_suggestion = match.value;
            errored = true;
        } else {
            append_argv_error(out_errors, st->idx, error_option_has_missing_argument, "Option expects an argument");
            errored = true;
        }
    }
    
    if (! errored) {
        // Construct resolved options
        for (size_t i=0; i < options_for_argument.size(); i++) {
            // Most options have no value.
            size_t val_idx = npos;
            rstring_t value;
            if (i + 1 == options_for_argument.size() && last_option_has_argument) {
                // This is the last option
                value = value_for_last_option;
                val_idx = val_idx_for_last_option;
            }
            const option_t &opt = options_for_argument.at(i);
            out_result->push_back(resolved_option_t(opt, name_idx, val_idx, value));
        }
        
        // Update the index
        st->idx += (last_option_has_argument ? 2 : 1);
    }
    return ! errored;
}


/* The Python implementation calls this "parse_argv" */
static void separate_argv_into_options_and_positionals(const rstring_list_t &argv, const option_list_t &options, parse_flags_t flags, positional_argument_list_t *out_positionals, resolved_option_list_t *out_resolved_options, error_list_t *out_errors, rstring_t *out_suggestion = NULL) {
    
    // double_dash means that all remaining values are arguments
    argv_separation_state_t st(argv, options, flags);
    while (st.idx < argv.size()) {
        if (st.saw_double_dash) {
            // double-dash means everything remaining is positional
            out_positionals->push_back(positional_argument_t(st.idx));
            st.idx += 1;
        } else if (st.has_double_dash_at(st.idx)) {
            // Literal --. The remaining arguments are positional.
            st.saw_double_dash = true;
            st.idx += 1;
        } else if (st.arg().has_prefix("--")) {
            // Leading long option
            if (parse_long(&st, option_t::double_long, out_resolved_options, out_errors, out_suggestion)) {
                // parse_long will have updated st.idx and out_resolved_options
            } else {
                // This argument is unused
                // We have to update idx
                st.idx += 1;
            }
        } else if (st.arg().has_prefix("-") && st.arg().length() > 1) {
            /* An option with a leading dash, like -foo
             This can be a lot of different things:
             1. A combined short option: tar -cf ...
             2. A long option with a single dash: -std=c++
             3. A short option with a value: -DNDEBUG
             Try to parse it as a long option; if that fails try to parse it as a short option.
             We cache the errors locally so that failing to parse it as a long option doesn't report an error if it parses successfully as a short option. This may result in duplicate error messages.
             */
            error_list_t local_long_errors, local_short_errors;
            if (parse_long(&st, option_t::single_long, out_resolved_options, &local_long_errors, out_suggestion)) {
                // parse_long succeeded
            } else if (parse_unseparated_short(&st, out_resolved_options, &local_short_errors, out_suggestion)) {
                // parse_unseparated_short will have updated idx and out_resolved_options
            } else if (parse_short(&st, out_resolved_options, &local_short_errors, out_suggestion)) {
                // parse_short succeeded.
            } else {
                /* Unparseable argument.
                 Say the user enters -Dfoo. This may be an unknown long option, or a short option with a value. If there is a short option -D, then it is more likely that the error from the short option parsing is what we want. So ensure the short erorrs appear at the front of the list. */
                if (out_errors) {
                    out_errors->insert(out_errors->begin(), local_long_errors.begin(), local_long_errors.end());
                    out_errors->insert(out_errors->begin(), local_short_errors.begin(), local_short_errors.end());
                }
                st.idx += 1;
            }
        } else {
            // Positional argument
            // Note this includes just single-dash arguments, which are often a stand-in for stdin
            out_positionals->push_back(positional_argument_t(st.idx));
            st.idx += 1;
        }
    }
}

#pragma mark -
#pragma mark Matching
#pragma mark -


/* The result of parsing argv */
typedef std::map<rstring_t, base_argument_t<rstring_t> > option_rmap_t;

struct match_state_t {
    // Map from option names to arguments
    option_rmap_t argument_values;
    
    // Next positional to dequeue
    size_t next_positional_index;
    
    // Bitset of options we've consumed
    std::vector<bool> consumed_options;
    
    std::set<rstring_t> suggested_next_arguments;
    
    // Whether this match has fully consumed all positionals and options
    bool fully_consumed;
    
    match_state_t() : next_positional_index(0), fully_consumed(false) {}
    
    void swap(match_state_t &rhs) {
        this->argument_values.swap(rhs.argument_values);
        this->consumed_options.swap(rhs.consumed_options);
        this->suggested_next_arguments.swap(rhs.suggested_next_arguments);
        std::swap(this->next_positional_index, rhs.next_positional_index);
        std::swap(this->fully_consumed, rhs.fully_consumed);
    }
    
    
    /* Returns the "progress" of a state. This is a sum of the number of positionals and arguments consumed, plus the number of suggestions. This is not directly comparable (two states may have identical progress values but be different) but it does capture the relationship between a state and another state derived from it, i.e. if the progress does not change then the child state is identical to its parent. */
    size_t progress() const {
        // Add in positionals
        size_t result = this->next_positional_index;
        
        // Add in arguments by counting set bits in the bitmap
        result += std::accumulate(this->consumed_options.begin(), this->consumed_options.end(), 0);
        
        // Add in number of suggestions
        result += suggested_next_arguments.size();
        
        return result;
    }
};

typedef std::vector<match_state_t> match_state_list_t;

struct match_context_t {
private:
    /** Returns true if the state has consumed all positionals and options */
    bool has_consumed_everything(const match_state_t *state) const {
        if (has_more_positionals(state)) {
            /* Unconsumed positional */
            return false;
        }
        for (std::vector<bool>::const_iterator iter = state->consumed_options.begin(); iter != state->consumed_options.end(); ++iter) {
            if (! *iter) {
                /* Unconsumed option */
                return false;
            }
        }
        return true;
    }
    
public:
    const parse_flags_t flags;
    
    /* Note: these are stored references. Match context objects are expected to be transient and stack-allocated. */
    const option_list_t &shortcut_options;
    const positional_argument_list_t &positionals;
    const resolved_option_list_t &resolved_options;
    const rstring_list_t &argv;
    
    bool has_more_positionals(const match_state_t *state) const {
        assert(state->next_positional_index <= this->positionals.size());
        return state->next_positional_index < this->positionals.size();
    }
    
    // Returns the indexes in argv of the arguments that were unused
    index_list_t unused_arguments(const match_state_t *state) const {
        /* To find the unused arguments, we walk over the used arguments and take what's left
         Arguments may be unused for any of three reasons:
         1. It is an unconsumed positional
         2. It is an option that we found in the tree, but was not matched during tree descent
         3. It is an option that was not found in the tree at all
         */
        
        /* Make a vector the same size as argv. As we walk over positionals and options, we will mark the corresponding index as used. At the end, the unset bits are the unused arguments */
        std::vector<bool> used_indexes(this->argv.size(), false);
        
        /* Walk over used positionals. next_positional_index is the first unused one.  */
        for (size_t i=0; i < state->next_positional_index; i++) {
            used_indexes.at(this->positionals.at(i).idx_in_argv) = true;
        }
        
        /* Walk over options matched during tree descent. We should have one bit per option */
        assert(state->consumed_options.size() == this->resolved_options.size());
        for (size_t i=0; i < state->consumed_options.size(); i++) {
            if (state->consumed_options.at(i)) {
                // This option was used. The name index is definitely used. The value index is also used, if it's not npos (note that it may be the same as the name index)
                const resolved_option_t &opt = this->resolved_options.at(i);
                used_indexes.at(opt.name_idx_in_argv) = true;
                if (opt.value_idx_in_argv != npos) {
                    used_indexes.at(opt.value_idx_in_argv) = true;
                }
            }
        }
        
        /* Walk over options NOT matched during tree descent and clear their bits. An argument may be both matched and unmatched, i.e. if "-vv" is parsed into two short options. In that case, we want to mark it as unmatched. */
        for (size_t i=0; i < state->consumed_options.size(); i++) {
            if (! state->consumed_options.at(i)) {
                const resolved_option_t &opt = this->resolved_options.at(i);
                used_indexes.at(opt.name_idx_in_argv) = false;
            }
        }
        
        /* Don't report the first -- as unused */
        for (size_t i=0; i < this->argv.size(); i++) {
            if (this->argv.at(i).is_double_dash()) {
                used_indexes.at(i) = true;
                break;
            }
        }
        
        /* Extract the unused indexes from the bitmap of used arguments */
        index_list_t unused_argv_idxs;
        for (size_t i=0; i < used_indexes.size(); i++) {
            if (! used_indexes.at(i)) {
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
    
    match_context_t(parse_flags_t f, const option_list_t &shortcut_opts, const positional_argument_list_t &p, const resolved_option_list_t &r, const rstring_list_t &av) : flags(f), shortcut_options(shortcut_opts), positionals(p), resolved_options(r), argv(av)
    {}
    
    /* If we want to stop a search and this state has consumed everything, stop the search */
    void try_mark_fully_consumed(match_state_t *state) {
        if ((this->flags & flag_stop_after_consuming_everything) && this->has_consumed_everything(state)) {
            state->fully_consumed = true;
        }
    }
};

// TODO: yuck
static void state_destructive_append_to(match_state_t *state, match_state_list_t *dest) {
    dest->resize(dest->size() + 1);
    dest->back().swap(*state);
}

static void state_append_to(const match_state_t *state, match_state_list_t *dest) {
    dest->resize(dest->size() + 1);
    dest->back() = *state;
}

static void match(const vector<usage_t> &usages, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const usage_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const expression_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const alternation_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const expression_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const simple_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const option_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const fixed_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const variable_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);


// TODO: comment me
template<typename T>
static void match_list(const T& node, match_state_list_t *incoming_state_list, match_context_t *ctx, match_state_list_t *resulting_states, bool require_progress = false) {
    if (! incoming_state_list->empty()) {
        for (size_t i=0; i < incoming_state_list->size(); i++) {
            match_state_t *state = &incoming_state_list->at(i);
            /* If we require that this makes progress, then get the current progress so we can compare */
            size_t init_progress = npos;
            size_t init_size = -1;
            if (require_progress) {
                init_progress = state->progress();
                init_size = resulting_states->size();
            }
            
            match(node, state, ctx, resulting_states);
            
            if (require_progress) {
                /* Keep only those results that have increased in progress. States after init_size are new. */
                size_t idx = resulting_states->size();
                assert(idx >= init_size);
                while (idx-- > init_size) { // last processed idx will be init_size
                    size_t new_progress = resulting_states->at(idx).progress();
                    assert(new_progress >= init_progress); // should never go backwards
                    if (new_progress == init_progress) {
                        // No progress was made, toss this state
                        resulting_states->erase(resulting_states->begin() + idx);
                    }
                }
            }
        }
    }
}


/* Match overrides */
static void match(const vector<usage_t> &usages, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    // Elide the copy in the last one
    size_t count = usages.size();
    if (count == 0) {
        return;
    }
    
    bool fully_consumed = false;
    for (size_t i=0; i + 1 < count && ! fully_consumed; i++) {
        match_state_t copied_state = *state;
        match(usages.at(i), &copied_state, ctx, resulting_states);
        
        if (ctx->flags & flag_stop_after_consuming_everything) {
            size_t idx = resulting_states->size();
            while (idx--) {
                if (resulting_states->at(idx).fully_consumed) {
                    fully_consumed = true;
                    break;
                }
            }
        }
    }
    if (! fully_consumed) {
        match(usages.at(count-1), state, ctx, resulting_states);
    }
}

static void match(const usage_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    if (! ctx->has_more_positionals(state)) {
        // todo: error handling
        return;
    }
    
    if (node.prog_name.empty()) {
        // This is the final terminating usage. It does not match anything.
        return;
    }
    
    // Program name
    ctx->acquire_next_positional(state);
    
    // Match against our contents
    match(node.alternation_list, state, ctx, resulting_states);
}

static void match(const expression_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    size_t count = node.expressions.size();
    if (count == 0) {
        // Merely append this state
        state_destructive_append_to(state, resulting_states);
    } else if (count == 1) {
        // Just one expression, trivial
        match(node.expressions.at(0), state, ctx, resulting_states);
    } else {
        // First expression
        match_state_list_t intermed_state_list;
        match(node.expressions.at(0), state, ctx, &intermed_state_list);
        // Middle expressions
        for (size_t i=1; i + 1 < count; i++) {
            match_state_list_t new_states;
            match_list(node.expressions.at(i), &intermed_state_list, ctx, &new_states);
            intermed_state_list.swap(new_states);
        }
        // Last expression
        match_list(node.expressions.at(count-1), &intermed_state_list, ctx, resulting_states);
    }
}

static void match(const alternation_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    size_t count = node.alternations.size();
    if (count == 0) {
        return;
    }
    for (size_t i=0; i + 1 < count; i++) {
        match_state_t copied_state = *state;
        match(node.alternations.at(i), &copied_state, ctx, resulting_states);
    }
    match(node.alternations.at(count-1), state, ctx, resulting_states);
}

static bool match_options(const option_list_t &options_in_doc, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states);
static void match(const expression_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    // Check to see if we have ellipsis. If so, we keep going as long as we can.
    bool has_ellipsis = node.opt_ellipsis.present;
    
    switch (node.production) {
        case 0:
        {
            /* This is a simple clause which may have ellipsis, like foo...
             If we have ellipsis, we match one time, and then construct a sequence of 'intermediate state lists'.
             An intermediate state represents the result of matching N times. Each time we construct
             a new intermediate state, we append (copy) all of its states into the result; thus we may
             match one time, two times, three times...
             We stop when we get no more matches, which usually happens when we run out of positionals.
             */
            assert(node.simple_clause.get() != NULL);
            size_t prior_state_count = resulting_states->size();
            match(*node.simple_clause, state, ctx, resulting_states);
            /* Now we know that all states starting at state_count_before are newly added. If we have ellipsis, go until we run out. */
            if (has_ellipsis) {
                while (prior_state_count < resulting_states->size()) {
                    match_state_list_t intermediate_states(resulting_states->begin() + prior_state_count, resulting_states->end());
                    prior_state_count = resulting_states->size();
                    match_list(*node.simple_clause, &intermediate_states, ctx, resulting_states, true /* require progress */);
                }
            }
            break;
        }
            
        case 1:
        {
            /* This is a parenthesized clause which may have ellipsis, like (foo)...
             Same algorithm as the simple clause above.
             TODO: this may loop forever with states that do not consume any values, e.g. ([foo])...
             */
            size_t prior_state_count = resulting_states->size();
            assert(node.alternation_list.get() != NULL);
            match(*node.alternation_list, state, ctx, resulting_states);
            if (has_ellipsis) {
                while (prior_state_count < resulting_states->size()) {
                    match_state_list_t intermediate_states(resulting_states->begin() + prior_state_count, resulting_states->end());
                    prior_state_count = resulting_states->size();
                    match_list(*node.alternation_list, &intermediate_states, ctx, resulting_states, true /* require progress */);
                }
            }
            break;
        }
            
        case 2:
        {
            /* This is a square-bracketed clause which may have ellipsis, like [foo]...
             Same algorithm as the simple clause above, except that we also append the initial state as a not-taken branch.
             */
            assert(node.alternation_list.get() != NULL);
            state_append_to(state, resulting_states);  // append the not-taken-branch
            size_t prior_state_count = resulting_states->size();
            match(*node.alternation_list, state, ctx, resulting_states);
            if (has_ellipsis) {
                while (prior_state_count < resulting_states->size()) {
                    match_state_list_t intermediate_states(resulting_states->begin() + prior_state_count, resulting_states->end());
                    prior_state_count = resulting_states->size();
                    match_list(*node.alternation_list, &intermediate_states, ctx, resulting_states, true /* require progress */);
                }
            }
            break;
        }
            
        case 3:
        {
            // This is the [options] clause. It does not have ellipsis.
            assert(node.options_shortcut.present);
            if (! match_options(ctx->shortcut_options, state, ctx, resulting_states)) {
                // No match, but matches are not required
                if (ctx->flags & flag_generate_suggestions) {
                    for (size_t i=0; i < ctx->shortcut_options.size(); i++) {
                        const option_t &opt = ctx->shortcut_options.at(i);
                        state->suggested_next_arguments.insert(opt.best_name());
                    }
                }
                state_destructive_append_to(state, resulting_states);
            }
            break;
        }
            
        default:
            assert(0 && "unknown production");
    }
}

// Match the options in the options list, updating the state
// This returns true if we match at least one
static bool match_options(const option_list_t &options_in_doc, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    bool successful_match = false;
    bool made_suggestion = false;
    
    // Collect potential suggestions in here. We squelch them if we find that a later matched option has the same corresponding long name; we need to remove those from the suggestions
    option_list_t potential_suggestions;
    
    for (size_t j=0; j < options_in_doc.size(); j++) {
        const option_t &opt_in_doc = options_in_doc.at(j);
        
        // Find the matching option from the resolved option list (i.e. argv)
        size_t resolved_opt_idx = npos;
        for (size_t i=0; i < ctx->resolved_options.size(); i++) {
            // Skip ones that have already been consumed
            if (! state->consumed_options.at(i)) {
                // See if the option from argv has the same key range as the option in the document
                if (ctx->resolved_options.at(i).option.has_same_name(opt_in_doc)) {
                    resolved_opt_idx = i;
                    break;
                }
            }
        }
        
        if (resolved_opt_idx != npos) {
            // We found a matching option in argv. Set it in the argument_values for this state and mark its index as used
            // We have two things to set:
            //  - The option name, like -foo
            //  - The option's argument value (if any)
            const resolved_option_t &resolved_opt = ctx->resolved_options.at(resolved_opt_idx);
            const rstring_t &name = opt_in_doc.best_name();
            
            // Update the option value, creating it if necessary
            state->argument_values[name].count += 1;
            
            // Update the option argument vlaue
            if (opt_in_doc.has_value() && resolved_opt.value_idx_in_argv != npos) {
                const rstring_t &variable_name = opt_in_doc.value;
                
                const rstring_t &value = resolved_opt.value_in_arg;
                state->argument_values[variable_name].values.push_back(value);
            }
            
            successful_match = true;
            state->consumed_options.at(resolved_opt_idx) = true;
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
        for (size_t i=0; i < potential_suggestions.size(); i++) {
            const option_t &suggestion = potential_suggestions.at(i);
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
        state_destructive_append_to(state, resulting_states);
    }
    return matched_something;
}

static void match(const simple_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    if (node.option.get()) {
        match(*node.option, state, ctx, resulting_states);
    } else if (node.fixed.get()) {
        match(*node.fixed, state, ctx, resulting_states);
    } else if (node.variable.get()) {
        match(*node.variable, state, ctx, resulting_states);
    } else {
        assert(0 && "Bug in docopt parser: No children of simple_clause.");
    }
}

static void match(const option_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    // Matching an option like --foo
    const option_list_t options_in_doc(1, node.option);
    bool matched = match_options(options_in_doc, state, ctx, resulting_states);
    if (! matched) {
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}

static void match(const fixed_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    // Fixed argument
    // Compare the next positional to this static argument
    if (ctx->has_more_positionals(state)) {
        const positional_argument_t &positional = ctx->next_positional(state);
        const rstring_t &name = ctx->argv.at(positional.idx_in_argv);
        if (node.word == rstring_t(name)) {
            // The static argument matches
            state->argument_values[name].count += 1;
            ctx->acquire_next_positional(state);
            ctx->try_mark_fully_consumed(state);
            state_destructive_append_to(state, resulting_states);
        }
    } else {
        // No more positionals. Maybe suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state->suggested_next_arguments.insert(node.word);
        }
        // Append the state if we are allowing incomplete
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}

static void match(const variable_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) {
    // Variable argument
    const rstring_t &name = node.word;
    if (ctx->has_more_positionals(state)) {
        // Note we retain the brackets <> in the variable name
        base_argument_t<rstring_t> *arg = &state->argument_values[name];
        const positional_argument_t &positional = ctx->acquire_next_positional(state);
        const rstring_t &positional_value = ctx->argv.at(positional.idx_in_argv);
        arg->values.push_back(positional_value);
        ctx->try_mark_fully_consumed(state);
        state_destructive_append_to(state, resulting_states);
    } else {
        // No more positionals. Suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state->suggested_next_arguments.insert(name);
        }
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}


template<typename stdstring_t>
base_argument_t<stdstring_t> finalize_argument(const base_argument_t<rstring_t> &arg) {
    base_argument_t<stdstring_t> result;
    result.count = arg.count;
    result.values.resize(arg.values.size());
    for (size_t i=0; i < arg.values.size(); i++) {
        arg.values[i].copy_to(&result.values[i]);
    }
    return result;
}


/* Wrapper class that takes either a string or wstring as string_t */
class docopt_impl {
    
#pragma mark -
#pragma mark Scanning
#pragma mark -
    
    /* Constructor takes the source in either narrow or wide form. */
public:
    /* Storage for our rstrings. Note that this must be shared_ptr so that we can have a sane copy constructor. Otherwise the copy constructor would copy our rstring_ts and have them pointing at the old docopt_impl! Plus this makes copying cheaper. */
    const shared_ptr<const std::string> storage_narrow;
    const shared_ptr<const std::wstring> storage_wide;
    const rstring_t rsource;
    docopt_impl(const std::string &s) : storage_narrow(new std::string(s)), rsource(*storage_narrow) {}
    docopt_impl(const std::wstring &s) : storage_wide(new std::wstring(s)), rsource(*storage_wide) {}
    
#pragma mark -
#pragma mark Instance Variables
#pragma mark -
    
    /* The usage parse tree. */
    usage_list_t usages;
    
    /* The list of options parsed from the "Options:" section. Referred to as "shortcut options" because the "[options]" directive can be used as a shortcut to reference them. */
    option_list_t shortcut_options;
    
    /* The list of options parsed from the "Options:" section and "Usage:" sections.  */
    option_list_t all_options;
    
    /* All of the variables that appear (like <kn>) from the "Usage:" sections */
    rstring_list_t all_variables;
    
    /* All of the positional commands (like "checkout") that appear in the "Usage:" sections */
    rstring_list_t all_static_arguments;
    
    /* Map from variable names to the commands that populate them */
    variable_command_map_t variables_to_commands;
    
    /* Walk over the lines of our source, starting from the beginning. */
    void populate_by_walking_lines(error_list_t *out_errors) {
        // TODO: needs rstring work
        /* Distinguish between normal (docopt) and exposition (e.g. description). */
        enum mode_t {
            mode_normal,
            mode_exposition
        } mode = mode_normal;
        
        // We need to parse the usage spec ranges after all of the Options
        // This is because we need the options to disambiguate some usages
        rstring_list_t usage_specs;
        
        rstring_t line;
        while (get_next_line(this->rsource, &line)) {
            /* There are a couple of possibilitise for each line:
             
             1. It may have a header like "Usage:". If so, we want to strip that header, and optionally
             use it to determine the mode.
             2. It may be a usage spec. We can tell because the first word is plain text.
             3. It may be an option spec. We can tell because the first character is a dash.
             4. It may be a variable spec. We can tell because the first character is a <.
             5. It may be just whitespace or empty, and is ignored.
             
             Also note that a (nonempty) line indented more than the previous line is considered a continuation of that line.
             */
            rstring_t trimmed_line = line.trim_whitespace();
            
            const rstring_t header = find_header(trimmed_line);
            if (!header.empty()) {
                // It's a header
                // Set mode based on header, and remove header from line
                // The headers we know about are Usage, Synopsis, Options, and Arguments (case insensitive)
                // Everything else is considered exposition
                const char * const keywords[] = { "Usage", "Synopsis", "Options", "Arguments" };
                size_t keyword_count = sizeof keywords / sizeof *keywords;
                bool found_keyword = false;
                for (size_t i=0; i < keyword_count && !found_keyword; i++) {
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
            
            /* Compute the indent. Note that the header is considered part of the indent, so that:
             
             Usage: foo
             bar
             
             Here 'foo' is indented more than 'bar'.
             */
            const size_t line_indent = compute_indent(line, trimmed_line);
            
            // Determine the "line group." That is, this line plus all subsequent nonempty lines
            // that are indented more than this line.
            rstring_t line_group = trimmed_line;
            rstring_t all_consumed_lines = line;
            rstring_t next_line = line;
            while (get_next_line(this->rsource, &next_line)) {
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
                this->shortcut_options.push_back(parse_one_option_spec(line_group, out_errors));
                
            } else if (first_char == '<') {
                // It's a variable command spec
                const variable_command_map_t new_var_cmds = parse_one_variable_command_spec(line_group, out_errors);
                for (variable_command_map_t::const_iterator iter = new_var_cmds.begin(); iter != new_var_cmds.end(); ++iter) {
                    if (!this->variables_to_commands.insert(*iter).second) {
                        append_docopt_error(out_errors, line_group, error_one_variable_multiple_commands, "Duplicate command for variable");
                    }
                }
                
            } else if (isalnum(first_char) || first_char == '_') {
                // It's a usage spec. We will come back to this.
                usage_specs.push_back(line_group);
                
            } else {
                // It's an error
                append_docopt_error(out_errors, trimmed_line, error_unknown_leader, "Lines must start with a normal character, less-than sign, or dash.");
                break;
            }
            
            // Note the line range we consumed, for the next iteration of the loop
            line = all_consumed_lines;
        }
        
        // Ensure our shortcut options don't have duplicates
        uniqueize_options(&this->shortcut_options, true /* error on duplicates */, out_errors);
        
        // Now parse our usage_spec_ranges
        size_t usages_count = usage_specs.size();
        this->usages.resize(usages_count);
        for (size_t i=0; i < usages_count; i++) {
            parse_one_usage(usage_specs.at(i), this->shortcut_options, &this->usages.at(i), out_errors);
        }
    }
    
    /* Given an option map (using rstring), convert it to an option map using the given std::basic_string type. */
    template<typename stdstring_t>
    typename argument_parser_t<stdstring_t>::argument_map_t finalize_option_map(const option_rmap_t &map, parse_flags_t flags) const {
        typename argument_parser_t<stdstring_t>::argument_map_t result;
        // Turn our string_ts into std::strings
        for (option_rmap_t::const_iterator iter = map.begin(); iter != map.end(); ++iter) {
            result[iter->first.std_string<stdstring_t>()] = finalize_argument<stdstring_t>(iter->second);
        }
        
        // Handle empty args
        if (flags & flag_generate_empty_args) {
            // For each option, fill in the value in the map
            // This could be made more efficient via a single call to insert()
            for (size_t i=0; i < all_options.size(); i++) {
                const option_t &opt = all_options.at(i);
                const rstring_t &name = opt.best_name();
                // We merely invoke operator[]; this will do the insertion with a default value if necessary.
                // Note that this is somewhat nasty because it may unnecessarily copy the key. We might use a find() beforehand to save memory
                result[name.std_string<stdstring_t>()];
                
                if (opt.has_value() && ! opt.default_value.empty()) {
                    // Maybe apply the default value for the variable
                    stdstring_t variable_name = opt.value.std_string<stdstring_t>();
                    base_argument_t<stdstring_t> *var_arg = &result[variable_name];
                    if (var_arg->values.empty()) {
                        var_arg->values.push_back(opt.default_value.std_string<stdstring_t>());
                    }
                }
            }
            
            // Fill in variables
            stdstring_t name;
            for (size_t i=0; i < all_variables.size(); i++) {
                all_variables.at(i).copy_to(&name);
                // As above, we merely invoke operator[]
                result[name];
            }
            
            // Fill in static arguments
            for (size_t i=0; i < all_static_arguments.size(); i++) {
                all_static_arguments.at(i).copy_to(&name);
                // As above, we merely invoke operator[]
                result[name];
            }
        }
        
        return result;
    }

    /* Matches argv */
    void match_argv(const rstring_list_t &argv,
                    parse_flags_t flags,
                    const positional_argument_list_t &positionals,
                    const resolved_option_list_t &resolved_options,
                    option_rmap_t *out_option_map,
                    index_list_t *out_unused_arguments,
                    bool log_stuff = false) const {
        /* Set flag_stop_after_consuming_everything. This allows us to early-out. */
        match_context_t ctx(flags | flag_stop_after_consuming_everything, this->shortcut_options, positionals, resolved_options, argv);
        match_state_t init_state;
        init_state.consumed_options.resize(resolved_options.size(), false);
        
        match_state_list_t result;
        match(this->usages, &init_state, &ctx, &result);
        
        if (log_stuff) {
            fprintf(stderr, "Matched %lu way(s)\n", result.size());
            for (size_t i=0; i < result.size(); i++) {
                const match_state_t &state = result.at(i);
                bool is_incomplete = ! ctx.unused_arguments(&state).empty();
                std::cerr <<  "Result " << i << (is_incomplete ? " (INCOMPLETE)" : "") << ":\n";
                for (option_rmap_t::const_iterator iter = state.argument_values.begin(); iter != state.argument_values.end(); ++iter) {
                    const rstring_t &name = iter->first;
                    const base_argument_t<rstring_t> &arg = iter->second;
                    fprintf(stderr, "\t%ls: ", name.std_string<std::wstring>().c_str());
                    for (size_t j=0; j < arg.values.size(); j++) {
                        if (j > 0) {
                            fprintf(stderr, ", ");
                        }
                        fprintf(stderr, "%ls", arg.values.at(j).std_string<std::wstring>().c_str());
                    }
                    std::cerr << '\n';
                }
            }
        }
        
        // Determine the index of the one with the fewest unused arguments
        size_t best_state_idx = npos;
        index_list_t best_unused_args;
        for (size_t i=0; i < result.size(); i++) {
            const match_state_t &state = result.at(i);
            index_list_t unused_args = ctx.unused_arguments(&state);
            size_t unused_arg_count = unused_args.size();
            if (i == 0 || unused_arg_count < best_unused_args.size()) {
                best_state_idx = i;
                best_unused_args.swap(unused_args);
                // If we got zero, we're done
                if (unused_arg_count == 0) {
                    break;
                }
            }
        }
        
        // Now return the winning state and its unused arguments
        if (best_state_idx != npos) {
            // We got a best state
            if (out_unused_arguments != NULL) {
                out_unused_arguments->swap(best_unused_args);
            }
            if (out_option_map != NULL) {
                out_option_map->swap(result.at(best_state_idx).argument_values);
            }
        } else {
            // No states. Every argument is unused.
            if (out_unused_arguments != NULL) {
                out_unused_arguments->clear();
                for (size_t i=0; i < argv.size(); i++) {
                    out_unused_arguments->push_back(i);
                }
            }
            if (out_option_map != NULL) {
                out_option_map->clear();
            }
        }
    }
    
    /* Parses the docopt, etc. Returns true on success, false on error */
    bool preflight(error_list_t *out_errors) {
        // Populate our instance variables
        this->populate_by_walking_lines(out_errors);
        
        /* If we have no usage, apply the default one */
        if (this->usages.empty()) {
            this->usages.resize(1);
            this->usages.back().make_default();
        }
        
        // Extract options and variables from the usage sections
        option_list_t usage_options;
        collect_options_and_variables(this->usages, &usage_options, &this->all_variables, &this->all_static_arguments);
        
        // Combine these into a single list
        this->all_options.reserve(usage_options.size() + this->shortcut_options.size());
        this->all_options.insert(this->all_options.end(), usage_options.begin(), usage_options.end());
        this->all_options.insert(this->all_options.end(), this->shortcut_options.begin(), this->shortcut_options.end());
        uniqueize_options(&this->all_options, false /* do not error on duplicates */, out_errors);
        
        /* Hackish. Consider the following usage:
         usage: prog [options] [-a]
         options: -a
         
         invoked as: prog -a -a
         
         Naively we would expect options to match the first -a arg, and the -a from usage to match the second. But we don't. Instead, if an option appears explicitly in a usage pattern, we excise it from the shortcuts.
         
         What Python docopt does is assert, if an option appears anywhere in any usage, it may not be matched by [options]. This seems reasonable, because it means that that option has more particular use cases. So remove all items from shortcut_options() that appear in usage_options.
         
         TODO: this currently only removes the matched variant. For example, prog -a --alpha would still be allowed.
         */
        
        for (size_t i=0; i < this->shortcut_options.size(); i++) {
            const option_t &shortcut_opt = this->shortcut_options.at(i);
            for (size_t j=0; j < usage_options.size(); j++) {
                const option_t &usage_opt = usage_options.at(j);
                if (shortcut_opt.has_same_name(usage_opt)) {
                    // Remove this shortcut, and decrement the index to reflect the position shift of the remaining items
                    this->shortcut_options.erase(this->shortcut_options.begin() + i);
                    i-=1;
                    break;
                }
            }
        }
        
        
        // Example of how to dump
        if ((0)) {
            std::string dumped;
            for (size_t i=0; i < this->usages.size(); i++)
            {
                dumped += node_dumper_t::dump_tree(this->usages.at(i));
            }
            fprintf(stderr, "%s\n", dumped.c_str());
        }
        
        /* Successfully preflighted */
        return true;
    }
    
    // TODO: make this const by stop touching error_list
    void best_assignment_for_argv(const rstring_list_t &argv, parse_flags_t flags, error_list_t *out_errors, index_list_t *out_unused_arguments, option_rmap_t *out_option_map)
    {
        positional_argument_list_t positionals;
        resolved_option_list_t resolved_options;
        
        // Extract positionals and arguments from argv
        separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals, &resolved_options, out_errors);
        
        // Produce an option map
        this->match_argv(argv, flags, positionals, resolved_options, out_option_map, out_unused_arguments);
    }
    
    rstring_list_t suggest_next_argument(const rstring_list_t &argv, parse_flags_t flags) const
    {
        /* Set internal flags to generate suggestions */
        flags |= flag_generate_suggestions;
        
        positional_argument_list_t positionals;
        resolved_option_list_t resolved_options;
        rstring_t suggestion;
        separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals, &resolved_options, NULL /* errors */, &suggestion);
        
        /* If we got a suggestion, it means that the last argument was of the form --foo, where --foo wants a value. That's all we care about. */
        if (! suggestion.empty()) {
            return rstring_list_t(1, suggestion);
        }
        
        match_context_t ctx(flags, shortcut_options, positionals, resolved_options, argv);
        match_state_t init_state;
        init_state.consumed_options.resize(resolved_options.size(), false);
        match_state_list_t states;
        match(this->usages, &init_state, &ctx, &states);
        
        /* Find the state(s) with the fewest unused arguments, and then insert all of their suggestions into a list */
        rstring_list_t all_suggestions;
        size_t best_unused_arg_count = (size_t)-1;
        for (size_t i=0; i < states.size(); i++) {
            size_t count = ctx.unused_arguments(&states.at(i)).size();
            if (count < best_unused_arg_count) {
                best_unused_arg_count = count;
            }
        }
        for (size_t i=0; i < states.size(); i++) {
            const match_state_t &state = states.at(i);
            if (ctx.unused_arguments(&state).size() == best_unused_arg_count) {
                all_suggestions.insert(all_suggestions.end(), state.suggested_next_arguments.begin(), state.suggested_next_arguments.end());
            }
        }
        // Eliminate duplicates
        std::sort(all_suggestions.begin(), all_suggestions.end());
        all_suggestions.erase(std::unique(all_suggestions.begin(), all_suggestions.end()), all_suggestions.end());
        return all_suggestions;
    }
    
    rstring_t commands_for_variable(const rstring_t &var_name) const {
        rstring_t result;
        variable_command_map_t::const_iterator where = this->variables_to_commands.find(var_name);
        if (where != this->variables_to_commands.end()) {
            result = where->second;
        }
        return result;
    }
    
    rstring_t description_for_option(const rstring_t &given_option_name) const {
        if (given_option_name.length() < 2 || given_option_name.at(0) != '-')
        {
            return rstring_t();
        }
        
        rstring_t result;
        const bool has_double_dash = (given_option_name.at(1) == '-');
        // We have to go through our options and compare their names to the given string
        const rstring_t needle(given_option_name);
        for (size_t i=0; i < this->all_options.size(); i++) {
            const option_t &opt = this->all_options.at(i);
            
            // We can skip options without descriptions
            if (opt.description.empty()) {
                continue;
            }
            
            bool matches = false;
            if (has_double_dash) {
                matches = (needle == opt.names[option_t::double_long]);
            } else {
                matches = (needle == opt.names[option_t::single_long] || needle == opt.names[option_t::single_short]);
            }
            
            if (matches) {
                result = opt.description;
                break;
            }
        }
        return result;
    }
    
    template<typename stdstring_t>
    std::vector<stdstring_t> get_command_names() const {
        /* Get the command names. We store a set of seen names so we only return tha names once, but in the order matching their appearance in the usage spec. */
        std::vector<stdstring_t> result;
        std::set<rstring_t> seen;
        for (size_t i=0; i < this->usages.size(); i++) {
            const usage_t &usage = this->usages.at(i);
            const rstring_t name = usage.prog_name;
            if (! name.empty() && seen.insert(name).second) {
                result.push_back(name.std_string<stdstring_t>());
            }
        }
        return result;
        
    }
    
    template<typename stdstring_t>
    std::vector<stdstring_t> get_variables() const {
        std::vector<stdstring_t> result;
        
        // Include explicit variables
        for (size_t i=0; i < this->all_variables.size(); i++) {
            const rstring_t &r = this->all_variables.at(i);
            result.push_back(r.std_string<stdstring_t>());
        }
        
        // Include variables that are part of options
        for (size_t i=0; i < this->all_options.size(); i++) {
            const rstring_t &r = this->all_options.at(i).value;
            if (! r.empty()) {
                result.push_back(r.std_string<stdstring_t>());
            }
        }
        
        // Sort and remove duplicates
        std::sort(result.begin(), result.end());
        result.erase(std::unique(result.begin(), result.end()), result.end());
        return result;
    }
    
}; // docopt_impl

template<typename stdstring_t>
std::vector<argument_status_t> argument_parser_t<stdstring_t>::validate_arguments(const std::vector<stdstring_t> &argv, parse_flags_t flags) const
{
    size_t arg_count = argv.size();
    std::vector<argument_status_t> result(arg_count, status_valid);
    
    index_list_t unused_args;
    const rstring_list_t argv_rstrs(argv.begin(), argv.end());
    impl->best_assignment_for_argv(argv_rstrs, flags, NULL /* errors */, &unused_args, NULL);
    
    // Unused arguments are all invalid
    for (size_t i=0; i < unused_args.size(); i++) {
        size_t unused_arg_idx = unused_args.at(i);
        result.at(unused_arg_idx) = status_invalid;
    }
    return result;
}

template<typename string_t>
std::vector<string_t> argument_parser_t<string_t>::suggest_next_argument(const std::vector<string_t> &argv, parse_flags_t flags) const
{
    const rstring_list_t argv_rstrs(argv.begin(), argv.end());
    rstring_list_t suggestions = impl->suggest_next_argument(argv_rstrs, flags);
    
    size_t length = suggestions.size();
    std::vector<string_t> result(length);
    for (size_t i=0; i < length; i++) {
        suggestions[i].copy_to(&result[i]);
    }
    return result;
}

template<typename stdstring_t>
stdstring_t argument_parser_t<stdstring_t>::commands_for_variable(const stdstring_t &var) const
{
    return impl->commands_for_variable(rstring_t(var)).std_string<stdstring_t>();
}

template<typename stdstring_t>
stdstring_t argument_parser_t<stdstring_t>::description_for_option(const stdstring_t &option) const
{
    return impl->description_for_option(rstring_t(option)).std_string<stdstring_t>();
}

template<typename stdstring_t>
std::vector<stdstring_t> argument_parser_t<stdstring_t>::get_command_names() const
{
    return impl->get_command_names<stdstring_t>();
}

template<typename stdstring_t>
std::vector<stdstring_t> argument_parser_t<stdstring_t>::get_variables() const
{
    return impl->get_variables<stdstring_t>();
}

template<typename stdstring_t>
typename argument_parser_t<stdstring_t>::argument_map_t
argument_parser_t<stdstring_t>::parse_arguments(const std::vector<stdstring_t> &argv,
                                                parse_flags_t flags,
                                                error_list_t *out_errors,
                                                std::vector<size_t> *out_unused_arguments) const {
    const rstring_list_t argv_rstrs(argv.begin(), argv.end());
    option_rmap_t option_rmap;
    impl->best_assignment_for_argv(argv_rstrs, flags, out_errors, out_unused_arguments, &option_rmap);
    return impl->finalize_option_map<stdstring_t>(option_rmap, flags);
}


template<typename stdstring_t>
bool argument_parser_t<stdstring_t>::set_doc(const stdstring_t &doc, error_list_t *out_errors) {
    docopt_impl *new_impl = new docopt_impl(doc);
    
    bool preflighted = new_impl->preflight(out_errors);
    
    if (! preflighted) {
        delete new_impl;
    } else {
        delete this->impl; // may be null
        this->impl = new_impl;
    }
    return preflighted;
}

/* Constructors */
template<typename string_t>
argument_parser_t<string_t>::argument_parser_t() : impl(NULL) {}

template<typename string_t>
argument_parser_t<string_t>::argument_parser_t(const string_t &doc, error_list_t *out_errors) : impl(NULL) {
    this->set_doc(doc, out_errors);
}

template<typename string_t>
argument_parser_t<string_t>::argument_parser_t(const argument_parser_t &rhs) {
    if (rhs.impl == NULL) {
        this->impl = NULL;
    } else {
        this->impl = new docopt_impl(*rhs.impl);
    }
}

template<typename string_t>
argument_parser_t<string_t> &argument_parser_t<string_t>::operator=(const argument_parser_t &rhs) {
    if (this != &rhs) {
        delete this->impl;
        if (rhs.impl == NULL) {
            this->impl = NULL;
        } else {
            this->impl = new docopt_impl(*rhs.impl);
        }
    }
    return *this;
}


/* Destructor */
template<typename string_t>
argument_parser_t<string_t>::~argument_parser_t<string_t>() {
    delete impl; // may be null
}

// close the namespace
CLOSE_DOCOPT_IMPL

// Force template instantiation
template class docopt_fish::argument_parser_t<std::string>;
template class docopt_fish::argument_parser_t<std::wstring>;


