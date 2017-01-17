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

// Class representing a metadata map. Keys are option names and variables.
typedef std::map<rstring_t, base_metadata_t<rstring_t>> metadata_map_t;

static inline std::basic_ostream<string_t::value_type> &errstream() {
#if DOCOPT_USE_WCHAR
    return std::wcerr;
#else
    return std::cerr;
#endif
}

#pragma mark -
#pragma mark Character classification
#pragma mark -


// Little helper function
bool is_double_dash(const string_t &str) {
    return str.size() == 2 && str[0] == '-' && str[1] == '-';
}

// Parsing helpers
template <char T>
bool it_equals(rstring_t::char_t c) {
    return c == T;
}

// Set of characters which
static bool char_is_valid_in_option_name(rstring_t::char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

static bool char_is_valid_in_variable_name(rstring_t::char_t c) {
    const char *invalid = "|()[]>\t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

bool char_is_space(rstring_t::char_t c) {
    return c == ' ';
}

#pragma mark -
#pragma mark Usage Spec Parsing
#pragma mark -

// This represents an error in the docopt specification itself
// The token is a substring of the docopt spec, and its location is used to
// determine the error
// position
static void append_token_error(error_list_t *errors, const rstring_t &token, int code,
                               const char *txt) {
    append_error(errors, token.offset(), code, txt, -1);
}

// Parses a variable name like '<foo>' from the remaining string
// Returns the variable name if we succeed, empty string if we fail
// Returns the remainder of the string by reference
static rstring_t parse_variable_name(rstring_t *inout_spec, error_list_t *out_errors) {
    assert(! inout_spec->empty());
    assert(inout_spec->front() == '<');
    rstring_t result;
    rstring_t remaining = *inout_spec;
    
    rstring_t open_bracket, name, close_bracket;
    std::tie(open_bracket, name, close_bracket) =
        remaining.scan_multiple<it_equals<'<'>, char_is_valid_in_variable_name, it_equals<'>'>>();
    assert(! open_bracket.empty());
    
    if (close_bracket.empty()) {
        // Invalid variable name, or missing > outright
        if (remaining.empty()) {
            // like '<foo'
            append_token_error(out_errors, open_bracket, error_missing_close_variable,
                               "No > to balance this <");
        } else {
            // like '<foo<'
            append_token_error(out_errors, remaining, error_invalid_variable_name,
                               "Character not allowed in variable name");
        }
    } else if (name.empty()) {
        append_token_error(out_errors, close_bracket, error_invalid_variable_name,
                           "Missing variable name");
    } else if (! remaining.empty() && char_is_valid_in_option_name(remaining.front())) {
        // Some extra stuff after the closing >
        append_token_error(out_errors, remaining, error_invalid_variable_name,
                           "Extra stuff after closing '>'");
    } else {
        result = open_bracket.merge(name).merge(close_bracket);
        *inout_spec = remaining;
    }
    return result;

}

// Given an inout string, parse out an option and return it by reference.
// Update the string to reflect the number of characters used.
bool option_t::parse_from_string(rstring_t *remaining, option_t *result, error_list_t *out_errors) {
    assert(!remaining->empty());
    error_list_t errors;
    
    // An option is one or more dashes, the option name, maybe space and/or equals
    rstring_t leading_dashes, name, space_separator, equals;
    std::tie(leading_dashes, name, space_separator, equals, std::ignore) =
        remaining->scan_multiple<it_equals<'-'>, char_is_valid_in_option_name, char_is_space,
                                 it_equals<'='>, char_is_space>();

    // Validate our fields
    const size_t dash_count = leading_dashes.length();
    assert(dash_count > 0);
    if (dash_count > 2) {
        append_token_error(&errors, leading_dashes, error_excessive_dashes, "Too many dashes");
    }
    
    if (name.empty()) {
        append_token_error(&errors, name, error_invalid_option_name, "Missing option name");
    }
    
    if (equals.length() > 1) {
        append_token_error(&errors, equals, error_excessive_equal_signs, "Too many equal signs");
    }
    
    // Maybe parse a variable name
    rstring_t var_name;
    if (remaining->has_prefix("<")) {
        var_name = parse_variable_name(remaining, out_errors);
    }
    
    // Report an error for cases like '--foo='
    if (var_name.empty() && !equals.empty()) {
        append_token_error(&errors, equals, error_invalid_variable_name,
                           "Missing variable for this assignment");
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

    // Generate an error on long options with no separators (--foo<bar>). Only
    // short options support these.
    if (type != single_short && ! var_name.empty() &&
        space_separator.empty() && equals.empty()) {
        append_token_error(&errors, name, error_bad_option_separator,
                           "Long options must use a space or equals separator");
    }
    
    bool success = errors.empty();
    if (success) {
        // Build flags
        option_flags_t flags = 0;
        if (type == single_long && ! equals.empty()) {
            // see SEPARATOR-NOTE
            flags |= single_long_strict_eqsep;
        }
        
        *result = option_t(type, leading_dashes.merge(name), var_name, flags);
    }
    if (out_errors) {
        std::move(errors.begin(), errors.end(), std::back_inserter(*out_errors));
    }
    return success;
}

// Given an argument (from argv) string, parse out an "option" of the given type
option_t option_t::parse_from_argument(const string_t &str, option_t::name_type_t type) {
    assert(!str.empty());
    assert(str.at(0) == '-');

    option_flags_t flags = 0;
    rstring_t remaining(str);

    // Get the name part
    const rstring_t dashes = remaining.scan_while<it_equals<'-'>>();
    rstring_t name, value;
    if (type == single_short) {
        // Short option, perhaps with unseparated value
        // here the name is just the next character (if any)
        // and the value is anything after it, including an = sign
        if (!remaining.empty()) {
            name = remaining.substr(0, 1);
            value = remaining.substr_from(1);
        }
    } else {
        // Long option
        name = remaining.scan_while<char_is_valid_in_option_name>();

        // Check to see if there's an = sign
        // If we got an equals sign, the rest is the value
        // It can have any character at all, since it's coming from the argument,
        // not from the usage spec
        const rstring_t equals = remaining.scan_1_char('=');
        if (!equals.empty()) {
            value = remaining;
            if (type == single_long) {
                flags |= single_long_strict_eqsep;
            }
        }
    }

    // Return the option
    return option_t(type, dashes.merge(name), value, flags);
}


// Helper to efficiently iterate over lines of a string 'base'. inout_line should be initially empty.
// On return, it will contain the line, with its end pointing just after the trailing newline, or
// possibly at the end.
// Returns true if a line was returned, false if we reached the end.
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

// Given an option spec, that extends from the initial - to the end of the
// description, parse out an option. Store descriptions in the given metadata.
// Note an option may have multiple names.
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
                append_token_error(errors, default_value, error_missing_close_bracket_in_default,
                                    "Missing ']' to match opening '['");
            } else {
                result.default_value = default_value.substr(0, default_value_end);
            }
        }
    }

    // Parse the options portion
    // This looks like '-f, --foo'
    rstring_t remaining = spec.substr(0, options_end).trim_whitespace();
    while (!remaining.empty()) {
        if (remaining[0] != '-') {
            append_token_error(errors, remaining, error_invalid_option_name, "Not an option");
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
        for (const rstring_t &name : result.names) {
            if (!name.empty()) {
                (*metadata)[name].description = description;
            }
        }
    }

    return result;
}

// Returns a 'header' in the given string, or an empty string if none.
// The "header" is the prefix ending with a colon, and only space / alpha text
// before it.
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
    rstring_t remaining = spec;
    rstring_t name = parse_variable_name(&remaining, out_errors);
    *out_variable_name = name;
    *out_command = remaining.trim_whitespace();
    return ! name.empty();
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
        const option_t representative = options->at(outer);

        // Find all options that share a name with this representative
        // Determine which one is best
        for (size_t match_cursor = outer + 1; match_cursor < options_count; match_cursor++) {
            option_t *candidate = &options->at(match_cursor);
            if (!representative.has_same_name(*candidate)) {
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
                append_token_error(errors, candidate->best_name(),
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

// Helper function to transform metadata from rstring_t to string_t
static metadata_t copyout_metadata(const base_metadata_t<rstring_t> &md) {
    metadata_t result;
    result.command = md.command.std_string();
    result.condition = md.condition.std_string();
    result.description = md.description.std_string();
    result.tag = md.tag;
    return result;
}

// Helper function to derive a suggestion from a key and its metadata
// Returns "empty" metadata if no metadata is stored for the given name
static suggestion_t suggestion_for(const rstring_t &name, const metadata_map_t &md_map, size_t offset = 0) {
    metadata_t md;
    auto suggestion_iter = md_map.find(name);
    if (suggestion_iter != md_map.end()) {
        md = copyout_metadata(suggestion_iter->second);
    }
    return suggestion_t(name.std_string(), std::move(md), offset);
}

// Object that knows how to separate argv into options and positionals
// This is stack-allocated and transient
class argv_classifier_t {
    // inputs to our separator
    const string_list_t &argv;
    const option_list_t &options;
    const metadata_map_t &names_to_metadata;
    const parse_flags_t flags;

    // separator internal state
    size_t idx = 0;
    
    // output of our separator
    arg_classification_t classification;
    suggestion_t suggestion;
    
    const string_t &arg() const {
        return this->argv.at(this->idx);
    }

    bool arg_has_prefix(const char *p) const {
        const string_t &arg = this->arg();
        const size_t len = strlen(p);
        return arg.size() >= len && std::equal(p, p + len, arg.begin());
    }

    // Returns the list of options satisfying some predicate
    template <typename UnaryFunc>
    std::vector<option_t> filter_options(const UnaryFunc &pred) const {
        std::vector<option_t> result;
        copy_if(this->options.begin(), this->options.end(), std::back_inserter(result), pred);
        return result;
    }

    bool parse_separated_option(option_t::name_type_t type, error_list_t *out_errors);
    bool parse_unseparated_short_options(error_list_t *out_errors);

    void suggest(const rstring_t &key) {
        if (this->flags & flag_generate_suggestions) {
            this->suggestion = suggestion_for(key, this->names_to_metadata);
        }
    }
    
    void error(error_list_t *out_errors, int code, const char *text, size_t pos_in_arg = 0) {
        append_error(out_errors, pos_in_arg, code, text, this->idx);
    }

    void parse_next_arg(error_list_t *out_errors);

    argv_classifier_t(const string_list_t &av, const option_list_t &opts,
                     const metadata_map_t &md_map, parse_flags_t fls)
    : argv(av), options(opts), names_to_metadata(md_map), flags(fls)
    {}
    
public:
    
    static arg_classification_t classify_arguments(const string_list_t &argv, const option_list_t &options,
                                                     const metadata_map_t &md_map, parse_flags_t flags,
                                                     error_list_t *out_errors, suggestion_t *out_suggestion = nullptr);

};

// Parse either a long value (type is single_long or double_long),
// or an unseparated short value like -DNDEBUG (type is single_short)
// We do NOT handle separated short values (-D NDEBUG) here;
// those are handled in parse_unseparated_short_options
bool argv_classifier_t::parse_separated_option(option_t::name_type_t type, error_list_t *out_errors) {
    const string_t &arg = this->arg();
    assert(this->arg_has_prefix(type == option_t::double_long ? "--" : "-"));

    // Parse the argument into an 'option'. Note that this option does not appear
    // in the options list because its range reflects the string in the argument.
    // TODO: Need to distinguish between equivalent ways of specifying parameters
    // (--foo=bar and --foo bar)
    option_t arg_as_option = option_t::parse_from_argument(arg, type);
    const rstring_t arg_name = arg_as_option.names[type];
    assert(!arg_name.empty());

    // Get list of matching options.
    // Short options must have a value to match, else we handle them in parse_unseparated_short_options
    auto match_iter = std::find_if(this->options.cbegin(), this->options.cend(), [&](const option_t &opt) {
        bool does_match = opt.names[type] == arg_name &&
            (type != option_t::single_short || opt.has_value());
        return does_match;
    });
    
    bool errored = false;
    // Our option de-duplication ensures we should never have more than one match
    if (match_iter == this->options.cend()) {
        this->error(out_errors, error_unknown_option, "Unknown option");
        errored = true;
    } else {
        const option_t &match = *match_iter;
        // Ensure the option and argument agree on having a value
        rstring_t value;
        const size_t name_idx = this->idx;
        size_t arg_index = npos;
        if (match.has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar (long) or -DNDEBUG (short)
                // The range is the value portion; the index is the same as our
                // argument.
                value = arg_as_option.value;
                arg_index = this->idx;
            } else if (! (match.flags & value_is_optional)) {
                if (this->idx + 1 < this->argv.size()) {
                    this->idx += 1;
                    arg_index = this->idx;
                    value = rstring_t(this->argv.at(arg_index));
                } else {
                    // We are at the last argument, and we expect a value. Return the
                    // value as a suggestion.
                    this->suggest(match.value);
                    this->error(out_errors, error_option_has_missing_argument,
                                      "Option expects an argument");
                    errored = true;
                }
            } else {
                // There was no value match, but it was optional
                assert(match.flags & value_is_optional);
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            this->error(out_errors, error_option_unexpected_argument,
                              "Option does not expect an argument");
            errored = true;
        }
        
        // Apply our SEPARATOR-NOTE
        // Be strict about = separators
        // Note that we use the single_long_strict_eqsep flag in arg_as_option
        // to indicate that we got an = separator
        if (! errored && type == option_t::single_long) {
            bool requires_eq = match.flags & single_long_strict_eqsep;
            bool has_eq = arg_as_option.flags & single_long_strict_eqsep;
            if (requires_eq != has_eq) {
                const char *msg = has_eq ?
                                   "Option requires a space to separate its argument" :
                                   "Option requires '=' to separate its argument";
                this->error(out_errors, error_wrong_separator, msg);
                errored = true;
            }
        }

        if (!errored) {
            this->classification.resolved_options.emplace_back(match, name_idx, arg_index, value);
            this->idx += 1;
        }
    }
    return ! errored;
}

// Given a list of short options, parse out arguments
// There may be multiple arguments, e.g. 'tar -xc'
// Only the last option may have an argument, e.g. 'tar -xcf somefile'
bool argv_classifier_t::parse_unseparated_short_options(error_list_t *out_errors) {
    const string_t &arg = this->arg();
    assert(this->arg_has_prefix("-"));
    assert(arg.length() > 1);  // must not be just a single dash
    bool errored = false;
    bool last_option_has_argument = false;

    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> options_for_argument;

    // walk over the characters in the argument, skipping the leading dash
    for (size_t idx_in_arg = 1; idx_in_arg < arg.length() && !errored; idx_in_arg++) {
        // Get list of short options matching this resolved option.
        const rstring_t::char_t short_char = arg.at(idx_in_arg);
        const option_list_t matches = this->filter_options([&](const option_t &opt) {
            // This is a short option (-D) so we check the second character (idx 1)
            const rstring_t &name = opt.names[option_t::single_short];
            return name.length() > 1 && name[1] == short_char;
        });
        
        size_t match_count = matches.size();
        // We should catch all duplicates during the preflight phase
        assert(match_count <= 1);
        if (match_count < 1) {
            this->error(out_errors, error_unknown_option, "Unknown short option",
                              idx_in_arg);
            errored = true;
        } else {
            // Just one match, add it to the global array
            options_for_argument.push_back(matches.at(0));
        }
    }

    if (!errored) {
        // Now we have all of the short options that this argument represents. No
        // option is allowed to have a value, except possibly the last one.
        for (size_t i = 0; i < options_for_argument.size(); i++) {
            const option_t &opt = options_for_argument.at(i);
            if (opt.has_value()) {
                if (i + 1 == options_for_argument.size()) {
                    // This is the last option
                    last_option_has_argument = true;
                } else {
                    // This is not the last option
                    // This i+1 is the position in the argument and needs some
                    // explanation. We have a leading dash and then an argument,
                    // which is parsed into short options - one per character, including
                    // the dash. Hence we can map from index-in-option to index-in-argument,
                    // unless there was an unknown option error above. In that case this
                    // will be wrong (but we typically only show the first error anyways)
                    this->error(out_errors, error_option_unexpected_argument,
                                      "Option may not have a value unless it is the last option",
                                      i + 1);
                    errored = true;
                }
            }
        }
    }

    // If we have an argument, determine the index of its name and its value
    rstring_t value_for_last_option;
    const size_t name_idx = this->idx;
    size_t val_idx_for_last_option = npos;
    if (!errored && last_option_has_argument) {
        // We don't support -f=bar style for short options.
        // TODO: support delimiter-free style (gcc -Dmacro=something)
        if (this->idx + 1 < this->argv.size()) {
            val_idx_for_last_option = this->idx + 1;
            value_for_last_option = rstring_t(this->argv.at(this->idx + 1));
        } else {
            // We are at the last argument, and we expect a value. Return the value as
            // a suggestion.
            this->suggest(options_for_argument.back().value);
            this->error(out_errors, error_option_has_missing_argument,
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
            this->classification.resolved_options.emplace_back(opt, name_idx, val_idx, value);
        }

        // Update the index
        this->idx += (last_option_has_argument ? 2 : 1);
    }
    return !errored;
}

void argv_classifier_t::parse_next_arg(error_list_t *out_errors) {
    assert(this->idx < argv.size());
    
    if (this->classification.double_dash_idx != npos) {
        // we saw a double-dash
        // everything remaining is positional
        this->classification.positionals.emplace_back(this->idx);
        this->idx += 1;
    } else if (is_double_dash(this->arg())) {
        // Literal --. The remaining arguments are positional.
        this->classification.double_dash_idx = this->idx;
        this->idx += 1;
    } else if (this->arg_has_prefix("--")) {
        // Leading long option
        if (this->parse_separated_option(option_t::double_long, out_errors)) {
            // this will have updated this->idx
        } else {
            // This argument is unused
            // We have to update idx
            this->idx += 1;
        }
    } else if (this->arg_has_prefix("-") && this->arg().length() > 1) {
        // An option with a leading dash, like -foo
        // This can be a lot of different things:
        // 1. A combined short option: tar -cf ...
        // 2. A long option with a single dash: -std=c++
        // 3. A short option with a value: -DNDEBUG
        // Try to parse it as a long option; if that fails try to parse it as a
        // short option. We cache the errors locally so that failing to parse it
        // as a long option doesn't report an error if it parses successfully
        // as a short option. This may result in duplicate error messages.
        error_list_t long_errors, short_errors;
        if (this->parse_separated_option(option_t::single_long, &long_errors)) {
            // successfully parsed as long option
        } else if (this->parse_separated_option(option_t::single_short, &short_errors)) {
            // parse_unseparated_short will have updated idx
        } else if (this->parse_unseparated_short_options(&short_errors)) {
            // parse_unseparated_short_options succeeded.
        } else {
            // Unparseable argument.
            // Say the user enters -Dfoo. This may be an unknown long option, or a
            // short option with a value. If there is a short option -D, then it is
            // more likely that the error from the short option parsing is what we
            // want. So append our short errors first.
            if (out_errors) {
                auto dst = std::back_inserter(*out_errors);
                std::move(short_errors.begin(), short_errors.end(), dst);
                std::move(long_errors.begin(), long_errors.end(), dst);
            }
            this->idx += 1;
        }
    } else {
        // Positional argument
        // Note this includes just single-dash arguments, which are often a
        // stand-in for stdin
        this->classification.positionals.emplace_back(this->idx);
        this->idx += 1;
    }
}

// The Python implementation calls this "parse_argv"
// Given an argv list, a set of options, and a set of flags,
// parse the argv into a set of positionals, options, errors, and optionally a suggestion
arg_classification_t argv_classifier_t::classify_arguments(const string_list_t &argv, const option_list_t &options,
                                                          const metadata_map_t &md_map, parse_flags_t flags,
                                                          error_list_t *out_errors, suggestion_t *out_suggestion) {
    argv_classifier_t st(argv, options, md_map, flags);
    while (st.idx < argv.size()) {
        st.parse_next_arg(out_errors);
    }
    
    if (out_suggestion)
        *out_suggestion = std::move(st.suggestion);
    
    return std::move(st.classification);
}

// This is the private implementation class of argument_parser_t
class docopt_impl {
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

    // The usage parse tree.
    std::vector<usage_t> usages;

    // When initialized via a usage spec (as a string), we acquire the string. Our
    // rstring_ts point into this.
    const string_t usage_storage;

    // List of direct options. Some of our rstrings point to strings inside of
    // these.
    const std::vector<annotated_option_t> annotated_options;

    // The list of options parsed from the "Options:" section. Referred to as
    // "shortcut options" because the "[options]" directive can be used as a shortcut
    // to reference them.
    option_list_t shortcut_options;

    // The list of options parsed from the "Options:" section and "Usage:"
    // sections.
    option_list_t all_options;

    // Map from variable/option names to their metadata
    metadata_map_t names_to_metadata;

   public:
    
    // Populate ourselves from our usage storage.
    void populate_from_usage(error_list_t *out_errors) {
        // Distinguish between normal (docopt) and exposition (e.g. description). */
        enum mode_t { mode_normal, mode_exposition } mode = mode_normal;

        // We need to parse the usage spec ranges after all of the Options
        // This is because we need the options to disambiguate some usages
        rstring_list_t usage_specs;

        rstring_t line;
        while (get_next_line(this->usage_storage, &line)) {
            // There are a couple of possibilitise for each line
            //
            // 1. It may have a header like "Usage:". If so, we want to strip that
            //    header, and optionally use it to determine the mode.
            // 2. It may be a usage spec. We can tell because the first word is plain text.
            // 3. It may be an option spec. We can tell because the first character is a dash.
            // 4. It may be a variable spec. We can tell because the first character is a <.
            // 5. It may be just whitespace or empty, and is ignored.
            //
            // Also note that a (nonempty) line indented more than the previous line is
            // considered a continuation of that line.
            rstring_t trimmed_line = line.trim_whitespace();
            const rstring_t header = find_header(trimmed_line);
            if (!header.empty()) {
                // It's a header
                // Set mode based on header, and remove header from line
                // The headers we know about are Usage, Synopsis, Options, and Arguments,
                // all case insensitive
                // Everything else is considered exposition and is ignored
                mode = mode_exposition;
                const char *const keywords[] = {"Usage", "Synopsis", "Options", "Arguments"};
                size_t keyword_count = sizeof keywords / sizeof *keywords;
                for (size_t i = 0; i < keyword_count; i++) {
                    if (header.find_case_insensitive(keywords[i]) != rstring_t::npos) {
                        mode = mode_normal;
                        break;
                    }
                }
                // Remove the header range from the trimmed line
                assert(header.length() <= trimmed_line.length());
                trimmed_line = trimmed_line.substr_from(header.length()).trim_whitespace();
            }

            // Skip exposition or empty lines
            if (mode == mode_exposition || trimmed_line.empty()) {
                continue;
            }

            // Compute the indent. Note that the header is considered part of the indent, so that:
            //
            //
            // Usage: foo
            // bar
            //
            // Here 'foo' is indented more than 'bar'.
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
                        append_token_error(out_errors, line_group,
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
                append_token_error(out_errors, trimmed_line, error_unknown_leader,
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
                this->shortcut_options.emplace_back(name_type, option_name,
                                                    value_name, dopt.flags);
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

    // Given an option map (using rstring), convert it to an option map using the
    // given std::basic_string type
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

    // Parses the docopt usage spec and sets up internal state.
    // Returns true on success, false on error
    bool preflight(error_list_t *out_errors) {
        // If we have no usage, apply the default one
        if (this->usages.empty()) {
            this->usages.push_back(usage_t::make_default());
        }

        // Extract options from the usage section
        option_list_t usage_options = collect_options(this->usages);

        // Combine these into a single list
        this->all_options.reserve(usage_options.size() + this->shortcut_options.size());
        this->all_options.insert(this->all_options.end(), usage_options.begin(),
                                 usage_options.end());
        this->all_options.insert(this->all_options.end(), this->shortcut_options.begin(),
                                 this->shortcut_options.end());
        uniqueize_options(&this->all_options, false /* do not error on duplicates */, out_errors);

        // Hackish. Consider the following usage:
         // usage: prog [options] [-a]
         // options: -a
         //
         // invoked as: prog -a -a
         //
         // Naively we would expect options to match the first -a arg, and the -a from
         // usage to match the second. But we don't. Instead, if an option appears explicitly
         // in a usage pattern, we excise it from the shortcuts.
         //
         // What Python docopt does is assert, if an option appears anywhere in any
         // usage, it may not be matched by [options]. This seems reasonable, because
         // it means that that option has more particular use cases. So remove all items
         // from shortcut_options() that appear in usage_options.
         //
         // TODO: this currently only removes the matched variant. For example,
         // prog -a --alpha
         // would still be allowed.
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
            for (const usage_t &usage : this->usages) {
                errstream() << dump_usage(usage) << "\n";
            }
        }

        // Successfully preflighted
        return true;
    }

    void best_assignment_for_argv(const string_list_t &argv, parse_flags_t flags,
                                  error_list_t *out_errors, index_list_t *out_unused_arguments,
                                  option_rmap_t *out_option_map) const {
        // Extract positionals and arguments from argv, then produce an option map
        arg_classification_t aclass = argv_classifier_t::classify_arguments(argv, all_options,
                                                                            names_to_metadata,
                                                                            flags, out_errors);
        
        // The flag_stop_after_consuming_everything allows us to early-out in the matching process
        flags |= flag_stop_after_consuming_everything;
        match_results_t mr = match_usages(this->usages, flags, this->shortcut_options, aclass, argv);
        if (out_unused_arguments) {
            *out_unused_arguments = std::move(mr.unused_args);
        }
        if (out_option_map) {
            *out_option_map = std::move(mr.option_map);
        }
    }

    // Given a list of suggestions and a partial argument, modify the suggestions by combining short options
    // with the partial argument
    // Example:
    //   Combinable short options -a, -b, -c
    //   Suggestions -a -b
    //   Partial argument -a
    //   Replaces with suggestion -ab
    // Note that we use the same metadata as the short option we're suggesting
    // Note that short options that take a value may only be combined as the last one (tar -cf <file>)
    void combine_short_suggestions(suggestion_list_t *suggestions, const string_t &partial_str) const {
        // Verify it starts with a - and at least one other character
        // Verify that every character in partial is a combinable short option in our suggestions
        // If so, append as a suggestion every combinable short option that doesn't appear in partial
        if (partial_str.length() <= 1 || partial_str.at(0) != '-') {
            return; // EARLY OUT
        }

        constexpr size_t short_option_name_length = 2;
        
        // Helper to return an option with the given short name
        auto short_option_named = [&](const string_t &name){
            const option_t *result = nullptr;
            if (name.length() == short_option_name_length) {
                const rstring_t rname(name);
                for (const option_t &opt : this->all_options) {
                    if (opt.names[option_t::single_short] == rname) {
                        result = &opt;
                        break;
                    }
                }
            }
            return result;
        };
        
        // Helper to indicate if the suggestion corresponds to a short option
        auto is_short_option_suggestion = [&](const suggestion_t &s) {
            return short_option_named(s.token) != nullptr;
        };
        
        
        // Generate map from character to suggestion for each short suggestion
        // e.g. an option '-e' would have an entry 'e' -> suggestion
        std::map<string_t::value_type, suggestion_t> char_to_suggestion;
        for (const suggestion_t &s : *suggestions) {
            if (is_short_option_suggestion(s)) {
                assert(s.token.length() == short_option_name_length);
                char_to_suggestion[s.token.at(1)] = s;
            }
        }
        
        // Verify that every character in the partial arg is in our suggestions,
        // and that the corresponding option does not require a value
        // If so, remove the corresponding suggestion (so that we only pick it up once)
        // Start at 1 to skip the leading dash
        // TODO: handle options with multiplicities (-vvv)
        bool partial_arg_is_valid_combined_shorts = true;
        for (rstring_t::char_t c : rstring_t(partial_str, 1)) {
            if (! char_to_suggestion.erase(c)) {
                // Not found in the map
                // So some character in the partial is not a suggestion
                partial_arg_is_valid_combined_shorts = false;
                break;
            }
            
            // Check for a value; short options with values must be last in a combined option,
            // and therefore cannot appear in partial strings
            // We should always have a short option, else it would not have been found
            // in char_to_suggestion
            const option_t *opt = short_option_named({'-', c});
            assert(opt != nullptr);
            if (opt->has_value()) {
                partial_arg_is_valid_combined_shorts = false;
                break;
            }
        }
        
        if (partial_arg_is_valid_combined_shorts) {
            // Whatever's left are our new suggestions
            // Erase all short suggestions, and append our new ones
            // New suggestions are the partial string, with each short option appended,
            // along with that short option's metadata
            suggestions->erase(std::remove_if(suggestions->begin(), suggestions->end(),
                                              is_short_option_suggestion),
                               suggestions->end());
            for (const auto &p : char_to_suggestion) {
                suggestions->emplace_back(partial_str + p.first, p.second.md);
            }
        }
    }
    
    // Given the partial last argument, suggest something to complete it
    // If the last argument is --foo=, where foo wants a value, we will suggest the variable
    // corresponding to the value
    suggestion_list_t suggest_value_completion(const string_t &partial_arg) const {
        if (partial_arg.size() < 2 || partial_arg.front() != '-')
            return {};
        
        suggestion_list_t suggestions;

        rstring_t long_name, short_name;
        const size_t short_name_len = 2; // like -D
        const size_t equals_pos = partial_arg.find('=');
        if (equals_pos != string_t::npos) {
            assert(equals_pos < partial_arg.size());
            // For example, we look like --foo=bar
            // Look for a value-carrying option with this long name (--foo)
            long_name = rstring_t(partial_arg, 0, equals_pos);
        } else {
            // We will try matching as an unseparated short option, like -D<Value>
            short_name = rstring_t(partial_arg, 0, short_name_len);
        }
        
        for (const option_t &opt : this->all_options) {
            if (! opt.has_value())
                continue;
            
            if (! long_name.empty()) {
                if (long_name == opt.names[option_t::single_long] ||
                    long_name == opt.names[option_t::double_long]) {
                    suggestions.push_back(suggestion_for(opt.value, this->names_to_metadata, equals_pos + 1));
                    break;
                }
            }
            
            if (! short_name.empty() && short_name == opt.names[option_t::single_short]) {
                suggestions.push_back(suggestion_for(opt.value, this->names_to_metadata, short_name_len));
                break;
            }
        }
        return suggestions;
    }
    
    suggestion_list_t suggest_next_argument(const string_list_t &argv_with_partial, parse_flags_t flags) const {
        // Set internal flags to generate suggestions
        flags |= flag_generate_suggestions;

        // The last argument is partial
        // Remove it and store it separately
        string_list_t argv = argv_with_partial;
        string_t partial_last_arg;
        if (! argv.empty()) {
            partial_last_arg = argv.back();
            argv.pop_back();
        }
        
        // Try completing a value for the last partial argument
        // This includes both --foo=bar... and -DNDEB...
        if (! partial_last_arg.empty()) {
            suggestion_list_t value_suggestions = this->suggest_value_completion(partial_last_arg);
            if (! value_suggestions.empty()) {
                return value_suggestions;
            }
        }
        
        suggestion_t suggestion;
        arg_classification_t aclass = argv_classifier_t::classify_arguments(argv, all_options, names_to_metadata,
                                                                           flags, nullptr /* errors */, &suggestion);

        // If we got a suggestion, it means that the last argument was of the form
        // --foo, where --foo wants a value. That's all we care about.
        if (!suggestion.token.empty()) {
            return {std::move(suggestion)};
        }
        
        // Perform the actual matching
        match_results_t mr = match_usages(this->usages, flags, this->shortcut_options, aclass, argv);
        
        // Turn the mr suggestion tokens into our annotated suggestions
        suggestion_list_t result;
        result.reserve(mr.suggestions.size());
        for (const rstring_t &tok : mr.suggestions) {
            result.push_back(suggestion_for(tok, this->names_to_metadata));
        }
        
        // Handle combined shorts with the partial argument
        if (! partial_last_arg.empty()) {
            combine_short_suggestions(&result, partial_last_arg);
        }
        
        return result;
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

suggestion_list_t argument_parser_t::suggest_next_argument(const string_list_t &argv,
                                                           parse_flags_t flags) const {
    return impl->suggest_next_argument(argv, flags);
}

metadata_t argument_parser_t::metadata_for_name(const string_t &var) const {
    const base_metadata_t<rstring_t> md = impl->metadata_for_name(rstring_t(var));
    return copyout_metadata(md);
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

// Constructors and destructors

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

argument_parser_t::~argument_parser_t() {
    delete impl;  // may be null
}

// close the namespace
CLOSE_DOCOPT_IMPL
