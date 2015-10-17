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

namespace docopt_fish
OPEN_DOCOPT_IMPL

static const size_t npos = (size_t)(-1);

// Narrow implementation
static inline size_t find_case_insensitive(const std::string &haystack, const char *needle, const range_t &haystack_range) {
    const size_t needle_len = strlen(needle);
    assert(needle_len > 0);
    if (needle_len > haystack_range.length) {
        // needle is longer than haystack, no possible match
        return std::wstring::npos;
    }
    size_t search_end = haystack_range.end() - needle_len;
    
    const char *haystack_cstr = haystack.c_str();
    const char first_down = tolower(needle[0]);
    const char first_up = toupper(needle[0]);
    for (size_t i = haystack_range.start; i <= search_end; i++) {
        // Common case
        char c = haystack_cstr[i];
        if (c != first_down && c != first_up) {
            continue;
        }
        if (0==strncasecmp(needle + 1, haystack_cstr + i + 1, needle_len - 1)) {
            return i;
        }
    }
    return std::string::npos;
}

// Nasty wide implementation
static inline size_t find_case_insensitive(const std::wstring &haystack, const char *needle, const range_t &haystack_range) {
    // Nasty implementation
    // The assumption here is that needle is always ASCII; thus it suffices to do an ugly tolower comparison
    assert(haystack_range.end() <= haystack.size());
    const size_t needle_len = strlen(needle);
    assert(needle_len > 0);
    if (needle_len > haystack_range.length) {
        // needle is longer than haystack, no possible match
        return std::wstring::npos;
    }

    const wchar_t *haystack_cstr = haystack.c_str();
    size_t search_end = haystack_range.end() - needle_len;
    const char first_down = tolower(needle[0]);
    const char first_up = toupper(needle[0]);
    
    for (size_t i=haystack_range.start; i <= search_end; i++) {
        // Common case
        wchar_t wc = haystack_cstr[i];
        if (wc != first_down && wc != first_up) {
            continue;
        }
        
        // See if we have a match at i
        size_t j;
        for (j = 1; j < needle_len; j++) {
            wchar_t wc = haystack_cstr[i + j];
            if (wc > 127 || tolower((char)wc) != tolower(needle[j])) {
                break;
            }
        }
        if (j == needle_len) {
            // Matched them all
            return i;
        }
    }
    return std::wstring::npos;
}

// This represents an error in argv, i.e. the docopt description was OK but a parameter contained an error
template <typename string_t>
static void append_argv_error(std::vector<error_t<string_t> > *errors, size_t arg_idx, int code, const char *txt, size_t pos_in_arg = 0) {
    append_error(errors, pos_in_arg, code, txt, arg_idx);
}


template<char T>
static bool it_equals(int c) { return c == T; }

template<typename char_t>
static bool char_is_valid_in_parameter(char_t c) {
    const char *invalid = ".|<>,=()[] \t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

template<typename char_t>
static bool char_is_valid_in_bracketed_word(char_t c) {
    const char *invalid = "|()[]>\t\n";
    const char *end = invalid + strlen(invalid);
    return std::find(invalid, end, c) == end;
}

template<typename string_t, typename T>
static range_t scan_while(const string_t &str, range_t *remaining, T func) {
    range_t result(remaining->start, 0);
    while (result.end() < remaining->end() && func(str.at(result.end()))) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}

// Returns a new range where leading and trailing whitespace has been trimmed
template<typename string_t>
static range_t trim_whitespace(const range_t &range, const string_t &src) {
    assert(range.end() <= src.size());
    size_t left = range.start, right = range.end();
    while (left < right && isspace(src.at(left)))
    {
        left++;
    }
    while (right > left && isspace(src.at(right - 1)))
    {
        right--;
    }
    assert(left <= right);
    return range_t(left, right - left);
}

template<typename string_t>
static range_t scan_1_char(const string_t &str, range_t *remaining, typename string_t::value_type c) {
    range_t result(remaining->start, 0);
    if (result.end() < remaining->end() && str.at(result.end()) == c) {
        result.length += 1;
        remaining->start += 1;
        remaining->length -= 1;
    }
    return result;
}


/* Given a string and the inout range 'remaining', parse out an option and return it. Update the remaining range to reflect the number of characters used. */
template<typename string_t>
option_t option_t::parse_from_string(const string_t &str, range_t *remaining, std::vector<error_t<string_t> >* errors UNUSED) {
    assert(remaining->length > 0);
    
    typedef typename string_t::value_type char_t;
    
    // Count how many leading dashes
    const size_t start = remaining->start;
    range_t leading_dash_range = scan_while(str, remaining, it_equals<'-'>);
    assert(leading_dash_range.length > 0);
    if (leading_dash_range.length > 2) {
        append_error(errors, start, error_excessive_dashes, "Too many dashes");
    }

    // Walk over characters valid in a name
    range_t name_range = scan_while(str, remaining, char_is_valid_in_parameter<char_t>);
    
    // Check to see if there's a space
    range_t space_separator = scan_while(str, remaining, isspace);

    // Check to see if there's an = sign
    const range_t equals_range = scan_while(str, remaining, it_equals<'='>);
    if (equals_range.length > 1) {
        append_error(errors, equals_range.start, error_excessive_equal_signs, "Too many equal signs");
    }

    // Try to scan a variable
    // TODO: If we have a naked equals sign (foo = ) generate an error
    scan_while(str, remaining, isspace);
    
    range_t variable_range;
    range_t open_sign = scan_1_char(str, remaining, '<');
    if (! open_sign.empty()) {
        range_t variable_name_range = scan_while(str, remaining, char_is_valid_in_bracketed_word<char_t>);
        range_t close_sign = scan_1_char(str, remaining, '>');
        if (variable_name_range.empty()) {
            append_error(errors, variable_name_range.start, error_invalid_variable_name, "Missing variable name");
        } else if (close_sign.empty()) {
            append_error(errors, open_sign.start, error_invalid_variable_name, "Missing '>' to match this '<'");
        } else {
            variable_range.merge(open_sign);
            variable_range.merge(variable_name_range);
            variable_range.merge(close_sign);
        }
        
        // Check to see what the next character is. If it's not whitespace or the end of the string, generate an error.
        if (! close_sign.empty() && ! remaining->empty() && char_is_valid_in_parameter(str.at(remaining->start))) {
            append_error(errors, remaining->start, error_invalid_variable_name, "Extra stuff after closing '>'");
        }
    }
    
    // Report an error for cases like --foo=
    if (variable_range.empty() && ! equals_range.empty()) {
        append_error(errors, equals_range.start, error_invalid_variable_name, "Missing variable for this assignment");
    }
    
    // Determine the separator type
    // If we got an equals range, it's something like 'foo = <bar>' or 'foo=<bar>'. The separator is equals and the space is ignored.
    // Otherwise, the space matters: 'foo <bar>' is space-separated, and 'foo<bar>' has no separator
    // Hackish: store sep_space for options without a separator
    option_t::separator_t separator;
    if (variable_range.empty()) {
        separator = option_t::sep_space;
    } else if (! equals_range.empty()) {
        separator = option_t::sep_equals;
    } else if (! space_separator.empty()) {
        separator = option_t::sep_space;
    } else {
        separator = option_t::sep_none;
    }
    
    // TODO: generate an error on long options with no separators (--foo<bar>). Only short options support these.
    if (separator == option_t::sep_none && (leading_dash_range.length > 1 || name_range.length > 1)) {
        append_error(errors, name_range.start, error_bad_option_separator, "Long options must use a space or equals separator");
    }
    
    // Generate errors for missing name
    if (name_range.empty()) {
        append_error(errors, name_range.start, error_invalid_option_name, "Missing option name");
    }
    
    // Create and return the option
    return option_t(name_range, variable_range, leading_dash_range.length, separator);
}


/* Wrapper template class that takes either a string or wstring as string_t */
template<typename string_t>
class docopt_impl OPEN_DOCOPT_IMPL

/* A character in string_t; likely either char or wchar_t */
typedef typename string_t::value_type char_t;

#pragma mark -
#pragma mark Scanning
#pragma mark -

/* Helper class for pretty-printing */
class node_dumper_t : public node_visitor_t<node_dumper_t> {
    string_t text;
    unsigned int depth;
    
    // Instances of this class are expected to be quite transient. src is unowned.
    const string_t *src;
    
    std::vector<std::string> lines;
    
    node_dumper_t(const string_t *src_ptr) : depth(0), src(src_ptr) {}
    
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
    
    void accept(const token_t &t1) {
        if (! t1.empty()) {
            std::string result(2 * depth, ' ');
            char buff[32];
            
            if (src != NULL) {
                const string_t word(*src, t1.range.start, t1.range.length);
                snprintf(buff, sizeof buff, "'%ls' ", widen(word).c_str());
            }
            result.append(buff);
            
            if (t1.range.length == 1) {
                snprintf(buff, sizeof buff, "{%lu}", t1.range.start);
            } else {
                snprintf(buff, sizeof buff, "{%lu-%lu}", t1.range.start, t1.range.length);
            }
            result.append(buff);
            lines.push_back(result);
        }
    }
    
    template<typename NODE_TYPE>
    static std::string dump_tree(const NODE_TYPE &node, const string_t &src) {
        node_dumper_t dumper(&src);
        dumper.begin(node);
        std::string result;
        for (size_t i=0; i < dumper.lines.size(); i++) {
            result.append(dumper.lines.at(i));
            result.push_back('\n');
        }
        dumper.src = NULL;
        return result;
    }
};


/* Helper class for collecting clauses from a tree */
struct clause_collector_t : public node_visitor_t<clause_collector_t> {
    option_list_t options;
    range_list_t fixeds;
    range_list_t variables;
    
    // The requested types we capture
    void accept(const option_clause_t& node) {
        options.push_back(node.option);
    }
    
    void accept(const fixed_clause_t& node) {
        fixeds.push_back(node.word.range);
    }
    
    void accept(const variable_clause_t& node) {
        variables.push_back(node.word.range);
    }
    
    // Other types we ignore
    template<typename IGNORED_TYPE>
    void accept(const IGNORED_TYPE& t UNUSED) {}
};

/* Class representing an error */
typedef std::vector<error_t<string_t> > error_list_t;

/* Class representing a map from variable names to conditions */
typedef std::map<string_t, range_t> variable_command_map_t;

/* List of usages */
typedef std::vector<usage_t> usage_list_t;

/* Constructor takes the source */
public:
const string_t source;
docopt_impl(const string_t &s) : source(s) {}

string_t string_for_range(const range_t &r) const
{
    assert(r.end() <= this->source.size());
    return string_t(this->source, r.start, r.length);
}

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
range_list_t all_variables;

/* All of the positional commands (like "checkout") that appear in the "Usage:" sections */
range_list_t all_static_arguments;

/* Map from variable names to the commands that populate them */
variable_command_map_t variables_to_commands;

/* Helper typedefs */
typedef base_argument_t<string_t> arg_t;
typedef std::vector<string_t> string_list_t;

/* A positional argument */
struct positional_argument_t {
    size_t idx_in_argv;

    explicit positional_argument_t(size_t idx) : idx_in_argv(idx)
    {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;

/* A resolved option references an option in argv */
struct resolved_option_t {

    // The option referenced by this
    option_t option;
    
    // The index of the name portion of the option, in argv
    size_t name_idx_in_argv;
    
    // The index of the argument where the value was found. npos for none.
    size_t value_idx_in_argv;
    
    // The range within that argument where the value was found. This will be the entire string if the argument is separate (--foo bar) but will be the portion after the equals if not (--foo=bar, -Dfoo)
    range_t value_range_in_arg;

    resolved_option_t(const option_t &opt, size_t name_idx, size_t val_idx, const range_t &val_range) : option(opt), name_idx_in_argv(name_idx), value_idx_in_argv(val_idx), value_range_in_arg(val_range)
    {}
};
typedef std::vector<resolved_option_t> resolved_option_list_t;

/* Helper function to efficiently iterate over lines of a string 'source'. On input, inout_range.end() should be initialized to the start point for the iteration. On return, the range will contain the line, with its end pointing just after the trailing newline, or possibly at source.size(). The length of the line is thus the length of the range (and is guaranteed to be positive). Returns true if a line was returned, false if we reached the end.

    If end is specified, it is treated as the end of the string (i.e. str.size())
*/
static bool get_next_line(const string_t &str, range_t *inout_range, size_t end = string_t::npos)
{
    const size_t effective_end = std::min(str.size(), end);
    bool success = false;
    if (inout_range->end() < effective_end)
    {
        // Start at the end of the last line, or zero if this is the first call
        size_t line_start = inout_range->end();
        size_t line_end;
        size_t newline = str.find(char_t('\n'), line_start);
        if (newline > effective_end) {
            // Point just after the last character
            line_end = effective_end;
        } else {
            // Point just after the newline
            line_end = newline + 1;
        }
        // Empty lines are impossible
        assert(line_end > line_start);
        inout_range->start = line_start;
        inout_range->length = line_end - line_start;
        success = true;
    }
    return success;
}

/* Returns true if the given range of this->source equals the given string. Optionally specify a start and length in the given string. */
bool range_equals_string(const range_t &range, const string_t &str, size_t start_in_str = 0, size_t len_in_str = npos) const {
    assert(range.end() <= this->source.size());
    assert(start_in_str <= str.size());
    // clamp length to the maximum allowed
    len_in_str = std::min(len_in_str, str.size() - start_in_str);
    bool result = false;
    if (range.length == len_in_str) {
        result = ! this->source.compare(range.start, range.length, str, start_in_str, len_in_str);
    }
    return result;
}

/* Returns true if the two strings are equal. */
static bool str_equals(const char *a, const string_t &str) {
    size_t len = str.size();
    const char_t *cstr = str.c_str();
    for (size_t i=0; i < len; i++) {
        // See if we've either reached the end of a, or the characters are different
        if (a[i] == '\0' || cstr[i] != a[i]) {
            return false;
        }
    }

    // We made it through. Ensure they have the same lengths
    // Note that we already dereferenced the previous characters of a
    return a[len] == '\0';
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const char_t *b, size_t len) {
    bool equals = true;
    for (size_t i=0; i < len; i++) {
        if (a[i] != b[i]) {
            equals = false;
            break;
        }
        if (a[i] == 0) {
            // End of string. Note that since these characters were equal, it's sufficient to test one of them.
            break;
        }
    }
    return equals;
}

/* Helper function for string equality. Returns true if a and b are equal up to the first len characters */
static bool substr_equals(const char *a, const string_t &b, size_t len) {
    return substr_equals(a, b.c_str(), len);
}

/* Returns true if the given token matches the given narrow string, up to len characters */
bool token_substr_equals(const token_t &tok, const char *str, size_t len) const {
    assert(tok.range.end() <= this->source.length());
    if (len > tok.range.length) {
        /* If our token is too short, then it doesn't match */
        return false;
    } else {
        return substr_equals(str, this->source.c_str() + tok.range.start, len);
    }
}

/* Collects options, i.e. tokens of the form --foo */
void collect_options_and_variables(const usage_list_t &usages, option_list_t *out_options, range_list_t *out_variables, range_list_t *out_static_arguments) const {
    clause_collector_t collector;
    for (size_t i=0; i < usages.size(); i++) {
        collector.begin(usages.at(i));
    }
    
    // "Return" the values
    out_options->swap(collector.options);
    out_variables->swap(collector.variables);
    out_static_arguments->swap(collector.fixeds);
}

/* Like parse_option_from_string, but parses an argument (as in argv) */
static option_t parse_option_from_argument(const string_t &str, error_list_t *errors UNUSED) {
    assert(! str.empty());
    assert(str.at(0) == char_t('-'));

    range_t remaining_storage(0, str.size());
    range_t * const remaining = &remaining_storage;
    
    // Count how many leading dashes
    range_t leading_dash_range = scan_while(str, remaining, it_equals<'-'>);
    
    // Walk over characters valid in a name
    range_t name_range = scan_while(str, remaining, char_is_valid_in_parameter<char_t>);
    
    // Check to see if there's an = sign
    const range_t equals_range = scan_1_char(str, remaining, char_t('='));
    
    // If we got an equals sign, the rest is the value
    // It can have any character at all, since it's coming from the argument, not from the usage spec
    range_t value_range;
    if (! equals_range.empty()) {
        value_range = *remaining;
        remaining->start = remaining->end();
        remaining->length = 0;
    }
    
    // Return the option
    return option_t(name_range, value_range, leading_dash_range.length, equals_range.empty() ? option_t::sep_space : option_t::sep_equals);
}

/* Given an option spec in the given range, that extends from the initial - to the end of the description, parse out a list of options */
option_list_t parse_one_option_spec(const range_t &range, error_list_t *errors) const {
    assert(! range.empty());
    assert(this->source.at(range.start) == char_t('-'));
    const size_t end = range.end();

    option_list_t result;

    // Look for two spaces. Those separate the description.
    // This is a two-space "C-string"
    const char_t two_spaces[] = {char_t(' '), char_t(' '), char_t('\0')};
    size_t options_end = this->source.find(two_spaces, range.start);
    if (options_end > end) {
        options_end = end; // no description
    }

    // Determine the description range (possibly empty). Trim leading and trailing whitespace
    range_t description_range = range_t(options_end, end - options_end);
    description_range = trim_whitespace(description_range, this->source);
    
    // Parse out a "default:" value.
    range_t default_value_range;
    if (! description_range.empty()) {
        // TODO: handle the case where there's more than one
        const char *default_prefix = "[default:";
        size_t default_prefix_loc = find_case_insensitive(this->source, default_prefix, description_range);
        if (default_prefix_loc != string_t::npos) {
            size_t default_value_start = default_prefix_loc + strlen(default_prefix);
            // Skip over spaces
            while (default_value_start < description_range.end() && isspace(this->source.at(default_value_start))) {
                default_value_start++;
            }
            
            // Find the closing ']'
            size_t default_value_end = this->source.find(char_t(']'), default_value_start);
            if (default_value_end >= description_range.end()) {
                // Note: The above check covers npos too
                append_error(errors, default_prefix_loc, error_missing_close_bracket_in_default, "Missing ']' to match opening '['");
            } else {
                default_value_range.start = default_value_start;
                default_value_range.length = default_value_end - default_value_start;
            }
        }
    }

    // Parse the options portion
    assert(options_end >= range.start);
    range_t remaining(range.start, options_end - range.start);
    scan_while(this->source, &remaining, isspace);
    range_t name_range_of_last_long_option;
    range_t last_value_range;
    while (! remaining.empty()) {
    
        if (this->source.at(remaining.start) != char_t('-')) {
            append_error(errors, remaining.start, error_invalid_option_name, "Not an option");
            break;
        }
    
        option_t opt = option_t::parse_from_string(this->source, &remaining, errors);
        if (opt.name.empty()) {
            // Failed to get an option, give up
            break;
        }
        opt.description_range = description_range;
        opt.default_value_range = default_value_range;
        result.push_back(opt);
        
        // Keep track of the last long option
        if (opt.type == option_t::double_long) {
            name_range_of_last_long_option = opt.name;
        }
        
        // Keep track of the last variable range
        // We imbue every other option with this range, unless it has its own
        if (! opt.value.empty()) {
            last_value_range = opt.value;
        }
        
        // Skip over commas, which separate arguments
        scan_while(this->source, &remaining, isspace);
        scan_while(this->source, &remaining, it_equals<','>);
        scan_while(this->source, &remaining, isspace);
    }
    
    // Set the corresponding long name to the name range of the last long option
    if (! name_range_of_last_long_option.empty()) {
        for (size_t i=0; i < result.size(); i++) {
            result.at(i).corresponding_long_name = name_range_of_last_long_option;
        }
    }
    
    // Set the value range of every option without one
    // This is to support use cases like this:
    //   -m, --message <contents>
    // The -m should pick up 'contents' too
    for (size_t i=0; i < result.size(); i++) {
        option_t *opt = &result.at(i);
        if (opt->value.empty()) {
            opt->value = last_value_range;
        }
    }
    
    return result;
}

// Computes the indent for a line starting at start and extending len. Tabs are treated as 4 spaces. newlines are unexpected, and treated as one space.
static size_t compute_indent(const string_t &src, size_t start, size_t len)
{
    const size_t tabstop = 4;
    assert(src.size() >= len);
    assert(start + len >= start); // no overflow
    size_t result = 0;
    for (size_t i=start; i < start + len; i++)
    {
        char_t c = src.at(i);
        if (c != L'\t')
        {
            // not a tab
            result += 1;
        }
        else
        {
            // is a tab. Round up to the next highest multiple of tabstop.
            // If we're already a multiple of tabstop, we want to go bigger.
            result = (result + tabstop) / tabstop * tabstop;
        }
    }
    return result;
}

static bool find_header(const string_t &src, const range_t &line_range, range_t *out_header_range) {
    /* We are considered a header if we contain a colon, and only space / alpha text before it. */
    bool result = false;
    for (size_t i=line_range.start; i < line_range.end(); i++) {
        char_t c = src[i];
        if (c == ':') {
            *out_header_range = range_t(line_range.start, i + 1 - line_range.start);
            result = true;
            break;
        } else if (! (isalnum(c) || c == ' ')) {
            break;
        }
    }
    return result;
}

/* Walk over the lines of our source, starting from the beginning. */
void populate_by_walking_lines(error_list_t *out_errors) {
    /* Distinguish between normal (docopt) and exposition (e.g. description). */
    enum mode_t {
        mode_normal,
        mode_exposition
    } mode = mode_normal;
    
    // We need to parse the usage spec ranges after all of the Options
    // This is because we need the options to disambiguate some usages
    range_list_t usage_spec_ranges;
    
    range_t line_range;
    while (get_next_line(this->source, &line_range)) {
        /* There are a couple of possibilitise for each line:
         
         1. It may have a header like "Usage:". If so, we want to strip that header, and optionally
            use it to determine the mode.
         2. It may be a usage spec. We can tell because the first word is plain text.
         3. It may be an option spec. We can tell because the first character is a dash.
         4. It may be a variable spec. We can tell because the first character is a <.
         5. It may be just whitespace or empty, and is ignored.
         
         Also note that a (nonempty) line indented more than the previous line is considered a continuation of that line.
         */
        
        range_t trimmed_line_range = trim_whitespace(line_range, this->source);
        assert(trimmed_line_range.start >= line_range.start);
        
        range_t header_range;
        if (find_header(this->source, trimmed_line_range, &header_range)) {
            // Set mode based on header, and remove header from line
            // The headers we know about are Usage, Synopsis, Options, and Arguments (case insensitive)
            // Everything else is considered exposition
            const char * const keywords[] = { "Usage", "Synopsis", "Options", "Arguments" };
            size_t keyword_count = sizeof keywords / sizeof *keywords;
            bool found_keyword = false;
            for (size_t i=0; i < keyword_count && !found_keyword; i++) {
                found_keyword = (find_case_insensitive(this->source, keywords[i], header_range) != string_t::npos);
            }
            mode = found_keyword ? mode_normal : mode_exposition;
            
            // Remove the header range from the trimmed line
            size_t header_end = header_range.end();
            assert(header_end <= trimmed_line_range.end());
            trimmed_line_range = trim_whitespace(range_t(header_end, trimmed_line_range.end() - header_end), this->source);
        }
        
        // Skip exposition or empty lines
        if (mode == mode_exposition || trimmed_line_range.empty()) {
            continue;
        }
        
        /* Compute the indent. Note that the header is considered part of the indent, so that:
         
          Usage: foo
              bar
        
          Here 'foo' is indented more than 'bar'.
        */
        const size_t line_indent = compute_indent(this->source, line_range.start, trimmed_line_range.start - line_range.start);
        
        // Determine the "line group." That is, this line plus all subsequent nonempty lines
        // that are indented more than this line.
        range_t line_group_range = trimmed_line_range;
        range_t all_consumed_lines = line_range;
        range_t next_line = line_range;
        while (get_next_line(this->source, &next_line)) {
            range_t trimmed_next_line = trim_whitespace(next_line, this->source);
            size_t next_line_indent = compute_indent(this->source, next_line.start, trimmed_next_line.start - next_line.start);
            if (trimmed_next_line.empty() || next_line_indent <= line_indent) {
                break;
            }
            line_group_range.merge(next_line);
            all_consumed_lines.merge(next_line);
        }
        
        char_t first_char = this->source.at(line_group_range.start);
        if (first_char == '-') {
            // It's an option spec
            option_list_t options = this->parse_one_option_spec(line_group_range, out_errors);
            this->shortcut_options.insert(this->shortcut_options.end(), options.begin(), options.end());
            
        } else if (first_char == '<') {
            // It's a variable command spec
            const variable_command_map_t new_var_cmds = parse_one_variable_command_spec(line_group_range, out_errors);
            for (typename variable_command_map_t::const_iterator iter = new_var_cmds.begin(); iter != new_var_cmds.end(); ++iter) {
                if (!this->variables_to_commands.insert(*iter).second) {
                    append_error(out_errors, line_group_range.start, error_one_variable_multiple_commands, "Duplicate command for variable");
                }
            }
            
        } else if (isalnum(first_char) || first_char == '_') {
            // It's a usage spec. We will come back to this.
            usage_spec_ranges.push_back(line_group_range);
            
        } else {
            // It's an error
            append_error(out_errors, trimmed_line_range.start, error_unknown_leader, "Lines must start with a normal character, less-than sign, or dash.");
            break;
        }
        
        // Note the line range we consumed, for the next iteration of the loop
        line_range = all_consumed_lines;
    }
    
    // Ensure our shortcut options don't have duplicates
    this->uniqueize_options(&this->shortcut_options, true /* error on duplicates */, out_errors);
    
    // Now parse our usage_spec_ranges
    size_t usages_count = usage_spec_ranges.size();
    this->usages.resize(usages_count);
    for (size_t i=0; i < usages_count; i++)
    {
        parse_one_usage<string_t>(this->source, usage_spec_ranges.at(i), this->shortcut_options, &this->usages.at(i), out_errors);
    }
}

/* Finds the headers containing name (for example, "Options:") and returns source ranges for them. Header lines are not included. We allow the section names to be indented, but must be less idented than the previous line. If include_unindented_lines is true, then non-header lines that are less indented are included:

  Usage: foo
  OTHER JUNK

With include_unindented_lines set to false, OTHER JUNK is treated as a header line and so not included in Usage:. If set to true, it is.

 */
range_list_t source_ranges_for_section(const char *name, bool include_other_top_level = false) const {
    range_list_t result;
    bool in_desired_section = false;
    range_t line_range;
    size_t current_header_indent = -1; //note: is huge
    while (get_next_line(this->source, &line_range)) {
        const range_t trimmed_line_range = trim_whitespace(line_range, this->source);
        assert(trimmed_line_range.start >= line_range.start);
        const size_t trimmed_line_start = trimmed_line_range.start;
        size_t line_indent = compute_indent(this->source, line_range.start, trimmed_line_start - line_range.start);
        
        // It's a header line if its indent is not greater than the previous header and it's empty
        size_t colon_pos = npos;
        bool is_header = false;
        bool is_other_top_level = false;
        if (! trimmed_line_range.empty() && line_indent <= current_header_indent) {
            colon_pos = find_colon(trimmed_line_range, this->source);
            is_header = (colon_pos != npos);
            is_other_top_level = (colon_pos == npos);
        }
        if (is_other_top_level && ! include_other_top_level) {
            // Other top level junk, end the section
            in_desired_section = false;
        } else if (is_header) {
            assert(colon_pos != npos && colon_pos >= line_range.start && colon_pos < line_range.end());
            current_header_indent = line_indent;
            
            // Check to see if the name is found before the first colon
            // Note that if name is not found at all, name_pos will have value npos, which is huge (and therefore not smaller than line_end)
            size_t name_pos = find_case_insensitive(source, name, trimmed_line_range);
            size_t line_end = trimmed_line_range.end();
            in_desired_section = (name_pos < line_end && name_pos < colon_pos);

            if (in_desired_section) {
                // Append a new empty range. We will add to it.
                result.push_back(range_t(0, 0));
                
                // Adjust this range to start after the header name
                size_t line_end = line_range.end();
                size_t new_start = name_pos + strlen(name);
                assert(new_start <= line_end);
                line_range.start = new_start;
                line_range.length = line_end - new_start;
            }
        }

        if (in_desired_section) {
            // Extend the last range with this line
            result.back().merge(line_range);
        }
    }
    return result;
}

/* Given a variable spec, parse out a condition map */
variable_command_map_t parse_one_variable_command_spec(const range_t &range, error_list_t *out_errors) const {
    // A specification look like this:
    // <pid> stuff
    variable_command_map_t result;
    assert(this->source.at(range.start) == '<');
    const size_t close_bracket = this->source.find('>', range.start);
    if (close_bracket >= range.end()) {
        // note: this covers npos too
        append_error(out_errors, range.start, error_missing_close_variable, "No > to balance this <");
    } else {
        assert(close_bracket < range.end());
        range_t key_range(range.start, close_bracket - range.start + 1);
        range_t value(key_range.end(), range.end() - key_range.end());
        key_range = trim_whitespace(key_range, this->source);
        value = trim_whitespace(value, this->source);
        result[string_for_range(key_range)] = value;
    }
    return result;
}

/* Returns true if the two options have the same name */
bool options_have_same_name(const option_t &opt1, const option_t &opt2) const {
    return opt1.has_same_name(opt2, this->source);
}

/* Given a list of options, verify that any duplicate options are in agreement, and remove all but one. TODO: make this not N^2. */
void uniqueize_options(option_list_t *options, bool error_on_duplicates, error_list_t *errors) const {
    std::vector<size_t> matching_indexes;
    for (size_t cursor=0; cursor < options->size(); cursor++) {
        // Determine the set of matches
        // Two options are called a match if they have the same name
        matching_indexes.push_back(cursor);
        // We have a "best option cursor". If we find two matching options, pick the one with the better (longer) description.
        size_t best_match_idx = cursor;
        for (size_t match_cursor = cursor + 1; match_cursor < options->size(); match_cursor++) {
            const option_t &current_match = options->at(best_match_idx);
            const option_t &maybe_match = options->at(match_cursor);
            if (this->options_have_same_name(current_match, maybe_match)) {
                if (error_on_duplicates) {
                    // Generate an error, and then continue on
                    append_error(errors, maybe_match.name.start, error_option_duplicated_in_options_section, "Option specified more than once");
                }
                // This index matched
                matching_indexes.push_back(match_cursor);
                
                // This argument matches.
                // TODO: verify agreement in the parameters, etc.
                if (maybe_match.description_range.length > current_match.description_range.length) {
                    // The second one has a better description. Keep it.
                    best_match_idx = match_cursor;
                }
            }
        }
        
        // Now we have the set of matching indexes
        // Erase all, except for the best match
        // These are in ascending order, so we can just work backwards
        while (! matching_indexes.empty()) {
            size_t idx_to_remove = matching_indexes.back();
            assert(idx_to_remove >= cursor); //should only remove at or after cursor
            matching_indexes.pop_back();
            if (idx_to_remove != best_match_idx) {
                options->erase(options->begin() + idx_to_remove);
                
                // If we removed the cursor, step back one so we go to the next element next time
                if (idx_to_remove == cursor) {
                    cursor -= 1;
                }
            }
        }
    }
}

/* Extracts a long option from the arg at idx, and appends the result to out_result. Updates idx.
TODO: merge with parse_unseparated_short, etc
*/
bool parse_long(const string_list_t &argv, option_t::type_t type, parse_flags_t flags, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors, string_t *out_suggestion = NULL) const {
    const string_t &arg = argv.at(*idx);
    assert(type == option_t::single_long || type == option_t::double_long);
    assert(substr_equals("--", arg, (type == option_t::double_long ? 2 : 1)));

    /* Parse the argument into an 'option'. Note that this option does not appear in the options list because its range reflects the string in the argument. TODO: Need to distinguish between equivalent ways of specifying parameters (--foo=bar and --foo bar) */
    error_list_t local_errors;
    option_t arg_as_option = parse_option_from_argument(arg, &local_errors);
    assert(arg_as_option.separator != option_t::sep_none);
    
    // Hacktastic - parse_option_from_string can't distinguish between one-char long options, and short options. So force the issue: if we want a single long option but we get a single short, then stomp it.
    if (type == option_t::single_long && arg_as_option.type == option_t::single_short) {
        arg_as_option.type = option_t::single_long;
    }
    
    // TODO: What if we get an error, e.g. there's more than two dashes?
    assert(arg_as_option.type == type);

    /* Get list of matching long options. */
    option_list_t matches;
    for (size_t i=0; i < options.size(); i++) {
        const option_t &opt = options.at(i);
        // This comparison is terrifying. It's just comparing two substrings: one in source (the given option) and the name portion of the argument
        if (opt.type == type && this->range_equals_string(opt.name, arg, arg_as_option.name.start, arg_as_option.name.length)) {
            // Should never have separator_none for long options
            assert(opt.separator != option_t::sep_none);
            matches.push_back(opt);
        }
    }
    
    if (matches.empty() && (flags & flag_resolve_unambiguous_prefixes)) {
        /* We didn't get any direct matches; look for an unambiguous prefix match */
        option_list_t prefix_matches;
        for (size_t i=0; i < options.size(); i++) {
            const option_t &opt = options.at(i);
            // Here we confirm that the option's name is longer than the name portion of the argument.
            // If they are equal; we would have had an exact match above; if the option is shorter, then the argument is not a prefix of it.
            // If the option is longer, we then do a substring comparison, up to the number of characters determined by the argument
            if (opt.type == type && opt.name.length > arg_as_option.name.length && this->source.compare(opt.name.start, arg_as_option.name.length, arg, arg_as_option.name.start, arg_as_option.name.length) == 0) {
                prefix_matches.push_back(opt);
            }
        }
        if (prefix_matches.size() > 1) {
            // Todo: list exactly the different options that this prefix can correspond to
            append_argv_error(out_errors, *idx, error_ambiguous_prefix_match, "Ambiguous prefix match");
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
        append_argv_error(out_errors, *idx, error_unknown_option, "Unknown long option");
    } else {
        bool errored = false;
        assert(match_count == 1);
        const option_t &match = matches.at(0);

        /* Ensure the option and argument agree on having a value */
        range_t value_range(0, 0);
        const size_t name_idx = *idx;
        size_t arg_index = npos;
        if (match.has_value()) {
            if (arg_as_option.has_value()) {
                // The arg was specified as --foo=bar. The range is the value portion; the index is the same as our argument.
                value_range = arg_as_option.value;
                arg_index = *idx;
            } else {
                // The arg was (hopefully) specified as --foo bar
                // The index is of the next argument, and the range is the entire argument
                if (*idx + 1 < argv.size()) {
                    *idx += 1;
                    arg_index = *idx;
                    value_range = range_t(0, argv.at(arg_index).size());
                } else if ((flags & flag_generate_suggestions) && out_suggestion != NULL) {
                    // We are at the last argument, and we expect a value. Return the value as a suggestion.
                    out_suggestion->assign(this->source, match.value.start, match.value.length);
                    errored = true;
                } else {
                    append_argv_error(out_errors, *idx, error_option_has_missing_argument, "Option expects an argument");
                    errored = true;
                }
            }
        } else if (arg_as_option.has_value()) {
            // A value was specified as --foo=bar, but none was expected
            append_argv_error(out_errors, *idx, error_option_unexpected_argument, "Option does not expect an argument");
            errored = true;
        }
        
        // If we want strict separators, check for separator agreement
        if (! errored && (flags & flag_short_options_strict_separators)) {
            if (arg_as_option.separator != match.separator) {
                // TODO: improve this error
                append_argv_error(out_errors, *idx, error_wrong_separator, "Option expects a different separator");
                errored = true;            
            }
        }
        
        if (! errored) {
            out_result->push_back(resolved_option_t(match, name_idx, arg_index, value_range));
            *idx += 1;
            success = true;
        }
    }
    return success;
}

// Given a list of short options, try parsing out an unseparated short, i.e. -DNDEBUG. We only look at short options with no separator. TODO: Use out_suggestion
bool parse_unseparated_short(const string_list_t &argv, parse_flags_t flags, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors, string_t *out_suggestion UNUSED) const {
    const string_t &arg = argv.at(*idx);
    assert(substr_equals("-", arg, 1));
    assert(arg.size() > 1); // must not be just a single dash
    bool success = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> matches;
    
    // If strict_separators is set, then we require that the option have sep_none
    // If not set, then we don't care if the separators match
    const bool relaxed_separators = ! (flags & flag_short_options_strict_separators);
    
    for (size_t i=0; i < options.size(); i++) {
        const option_t &opt = options.at(i);
        if (opt.type == option_t::single_short && opt.has_value() && (relaxed_separators || opt.separator == option_t::sep_none)) {
            // Candidate short option.
            // This looks something like -DNDEBUG. We want to see if the D matches.
            // Compare the character at offset 1 (to account for the dash) and length 1 (since it's a short option)
            if (this->range_equals_string(opt.name, arg, 1, 1)) {
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
        if (arg.size() <= 2) {
            append_argv_error(out_errors, *idx, error_option_has_missing_argument, "Option expects an argument");
        } else {
            // Got one
            size_t name_idx = *idx;
            size_t value_idx = *idx;
            range_t value_range = range_t(2, arg.size() - 2);
            out_result->push_back(resolved_option_t(match, name_idx, value_idx, value_range));
            *idx += 1;
            success = true;
        }
    } else {
        // Common case: Match count is 0, so we didn't get any.
        assert(matches.empty());
    }
    return success;
}


// Given a list of short options, parse out an argument
bool parse_short(const string_list_t &argv, parse_flags_t flags, size_t *idx, const option_list_t &options, resolved_option_list_t *out_result, error_list_t *out_errors, string_t *out_suggestion) const {
    const string_t &arg = argv.at(*idx);
    assert(substr_equals("-", arg, 1));
    assert(arg.size() > 1); // must not be just a single dash
    bool errored = false;
    bool last_option_has_argument = false;
    
    // Construct the list of options in-order, corresponding to this argument
    std::vector<option_t> options_for_argument;
    
    std::vector<option_t> matches;
    for (size_t idx_in_arg=1; idx_in_arg < arg.size() && ! errored; idx_in_arg++) {
        /* Get list of short options matching this resolved option. These are pointers into our options array */
        matches.clear();
        for (size_t i=0; i < options.size(); i++) {
            const option_t &opt = options.at(i);
            // This comparison is terrifying. It's just comparing two substrings: one in source (the given option) and the name portion of the argument. We pass 1 because the length of the string is 1.
            if (opt.type == option_t::single_short && this->range_equals_string(opt.name, arg, idx_in_arg, 1)) {
                matches.push_back(opt);
            }
        }
        
        size_t match_count = matches.size();
        // We should catch all duplicates during the preflight phase
        assert(match_count <= 1);
        if (match_count < 1) {
            append_argv_error(out_errors, *idx, error_unknown_option, "Unknown short option", idx_in_arg);
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
                    // This i+1 is the position in the argument and needs some explanation. Since we have a leading dash and then an argument, which is parsed into short options - one per character (except for the dash). Hence we can map from index-in-option to index-in-argument, unless there was an unknown option error above. In that case this will be wrong (but we typically only show the first error anyways).
                    append_argv_error(out_errors, *idx, error_option_unexpected_argument, "Option may not have a value unless it is the last option", i + 1);
                }
            }
        }
    }
    
    // If we have an argument, determine its index
    range_t val_range_for_last_option(0, 0);
    const size_t name_idx = *idx;
    size_t val_idx_for_last_option = npos;
    if (! errored && last_option_has_argument) {
        // We don't support -f=bar style. I don't know of any commands that use this.
        // TODO: support delimiter-free style (gcc -Dmacro=something)
        if (*idx + 1 < argv.size()) {
            val_idx_for_last_option = *idx + 1;
            val_range_for_last_option = range_t(0, argv.at(*idx + 1).size());
        } else if ((flags & flag_generate_suggestions) && out_suggestion != NULL) {
            // We are at the last argument, and we expect a value. Return the value as a suggestion.
            const option_t &match = options_for_argument.back();
            out_suggestion->assign(this->source, match.value.start, match.value.length);
            errored = true;
        } else {
            append_argv_error(out_errors, *idx, error_option_has_missing_argument, "Option expects an argument");
            errored = true;
        }
    }
    
    if (! errored) {
        // Construct resolved options
        for (size_t i=0; i < options_for_argument.size(); i++) {
            // Most options have no value.
            size_t val_idx = npos;
            range_t val_range(0, 0);
            if (i + 1 == options_for_argument.size() && last_option_has_argument) {
                // This is the last option
                val_range = val_range_for_last_option;
                val_idx = val_idx_for_last_option;
            }
            const option_t &opt = options_for_argument.at(i);
            out_result->push_back(resolved_option_t(opt, name_idx, val_idx, val_range));
        }
        
        // Update the index
        *idx += (last_option_has_argument ? 2 : 1);
    }
    return ! errored;
}

/* The Python implementation calls this "parse_argv" */
void separate_argv_into_options_and_positionals(const string_list_t &argv, const option_list_t &options, parse_flags_t flags, positional_argument_list_t *out_positionals, resolved_option_list_t *out_resolved_options, error_list_t *out_errors, string_t *out_suggestion = NULL) const {

    size_t idx = 0;
    while (idx < argv.size()) {
        const string_t arg = argv.at(idx);
        if (str_equals("--", arg)) {
            // Literal --. The remaining arguments are positional. Insert everything remaining and exit early
            while (++idx < argv.size()) {
                out_positionals->push_back(positional_argument_t(idx));
            }
            break;
        } else if (substr_equals("--", arg, 2)) {
            // Leading long option
            if (parse_long(argv, option_t::double_long, flags, &idx, options, out_resolved_options, out_errors, out_suggestion)) {
                // parse_long will have updated idx and out_resolved_options
            } else {
                // This argument is unused
                // We have to update idx
                idx += 1;
            }
        } else if (substr_equals("-", arg, 1) && arg.size() > 1) {
            /* An option with a leading dash, like -foo
             This can be a lot of different things:
               1. A combined short option: tar -cf ...
               2. A long option with a single dash: -std=c++
               3. A short option with a value: -DNDEBUG
             Try to parse it as a long option; if that fails try to parse it as a short option.
             We cache the errors locally so that failing to parse it as a long option doesn't report an error if it parses successfully as a short option. This may result in duplicate error messages.
             */
            error_list_t local_long_errors, local_short_errors;
            if (parse_long(argv, option_t::single_long, flags, &idx, options, out_resolved_options, &local_long_errors, out_suggestion)) {
                // parse_long succeeded
            } else if (parse_unseparated_short(argv, flags, &idx, options, out_resolved_options, &local_short_errors, out_suggestion)) {
                // parse_unseparated_short will have updated idx and out_resolved_options
            } else if (parse_short(argv, flags, &idx, options, out_resolved_options, &local_short_errors, out_suggestion)) {
                // parse_short succeeded.
            } else {
                /* Unparseable argument.
                Say the user enters -Dfoo. This may be an unknown long option, or a short option with a value. If there is a short option -D, then it is more likely that the error from the short option parsing is what we want. So ensure the short erorrs appear at the front of the list. */
                if (out_errors) {
                    out_errors->insert(out_errors->begin(), local_long_errors.begin(), local_long_errors.end());
                    out_errors->insert(out_errors->begin(), local_short_errors.begin(), local_short_errors.end());
                }
                idx += 1;
            }
        } else {
            // Positional argument
            // Note this includes just single-dash arguments, which are often a stand-in for stdin
            out_positionals->push_back(positional_argument_t(idx));
            idx += 1;
        }
    }
}

/* The result of parsing argv */
typedef std::map<string_t, base_argument_t<string_t> > option_map_t;

struct match_state_t {
    // Map from option names to arguments
    option_map_t argument_values;
    
    // Next positional to dequeue
    size_t next_positional_index;
    
    // Bitset of options we've consumed
    std::vector<bool> consumed_options;
    
    std::set<string_t> suggested_next_arguments;
    
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
    const positional_argument_list_t &positionals;
    const resolved_option_list_t &resolved_options;
    const string_list_t &argv;
    
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

    match_context_t(parse_flags_t f, const positional_argument_list_t &p, const resolved_option_list_t &r, const string_list_t &av) : flags(f), positionals(p), resolved_options(r), argv(av)
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


// TODO: comment me
template<typename T>
void match_list(const T& node, match_state_list_t *incoming_state_list, match_context_t *ctx, match_state_list_t *resulting_states, bool require_progress = false) const {
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
            
            this->match(node, state, ctx, resulting_states);
            
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
void match(const vector<usage_t> &usages, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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

/* Match overrides */
void match(const usage_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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

void match(const expression_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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

void match(const alternation_list_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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

void match(const expression_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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
            if (! this->match_options(this->shortcut_options, state, ctx, resulting_states)) {
                // No match, but matches are not required
                if (ctx->flags & flag_generate_suggestions) {
                    for (size_t i=0; i < this->shortcut_options.size(); i++) {
                        const option_t &opt = this->shortcut_options.at(i);
                        state->suggested_next_arguments.insert(opt.name_as_string(this->source));
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
bool match_options(const option_list_t &options_in_doc, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
    bool successful_match = false;
    bool made_suggestion = false;
    
    // As we traverse, ensure we don't match both -f and --foo by remembering the key ranges of the matched options
    range_list_t matched_long_ranges;
    
    // Collect potential suggestions in here. We squelch them if we find that a later matched option has the same corresponding long name; we need to remove those from the suggestions
    option_list_t potential_suggestions;
    
    for (size_t j=0; j < options_in_doc.size(); j++) {
        const option_t &opt_in_doc = options_in_doc.at(j);
        
        // Skip this option if its key range is already used
        const range_t &key_range = opt_in_doc.corresponding_long_name;
        if (! key_range.empty() && std::find(matched_long_ranges.begin(), matched_long_ranges.end(), key_range) != matched_long_ranges.end()) {
            continue;
        }

        // Find the matching option from the resolved option list (i.e. argv)
        size_t resolved_opt_idx = npos;
        bool option_already_consumed = false;
        for (size_t i=0; i < ctx->resolved_options.size(); i++) {
            // Skip ones that have already been consumed
            if (! state->consumed_options.at(i)) {
                // See if the option from argv has the same key range as the option in the document
                if (options_have_same_name(ctx->resolved_options.at(i).option, opt_in_doc)) {
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
            const string_t name = opt_in_doc.longest_name_as_string(this->source);
            
            // Update the option value, creating it if necessary
            state->argument_values[name].count += 1;
            
            // Update the option argument vlaue
            if (opt_in_doc.has_value() && resolved_opt.value_idx_in_argv != npos) {
                const string_t variable_name(this->source, opt_in_doc.value.start, opt_in_doc.value.length);
                
                const string_t &arg_with_value = ctx->argv.at(resolved_opt.value_idx_in_argv);
                const range_t value_range = resolved_opt.value_range_in_arg;
                state->argument_values[variable_name].values.push_back(string_t(arg_with_value, value_range.start, value_range.length));
            }
            
            successful_match = true;
            state->consumed_options.at(resolved_opt_idx) = true;
            if (! opt_in_doc.corresponding_long_name.empty()) {
                matched_long_ranges.push_back(opt_in_doc.corresponding_long_name);
            }
        } else if (! option_already_consumed) {
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
            const range_t &key_range = suggestion.corresponding_long_name;
            if (key_range.empty() || find(matched_long_ranges.begin(), matched_long_ranges.end(), key_range) == matched_long_ranges.end()) {
                // This option's long name was not found in the matched long names
                state->suggested_next_arguments.insert(suggestion.name_as_string(this->source));
                made_suggestion = true;
            }
        }
    }

    bool matched_something = successful_match || made_suggestion;
    if (matched_something) {
        ctx->try_mark_fully_consumed(state);
        state_destructive_append_to(state, resulting_states);
    }
    return matched_something;
}

void match(const simple_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
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

void match(const option_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
    // Matching an option like --foo
    const option_list_t options_in_doc(1, node.option);
    bool matched = this->match_options(options_in_doc, state, ctx, resulting_states);
    if (! matched) {
        // Didn't get any options. Maybe we suggest one.
        // We want to return this state if either we were in square brackets (so we don't require a match), OR we are accepting incomplete
        if (ctx->flags & flag_generate_suggestions) {
            state->suggested_next_arguments.insert(options_in_doc.back().name_as_string(this->source));
        }
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}

void match(const fixed_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
    // Fixed argument
    // Compare the next positional to this static argument
    const range_t &range = node.word.range;
    if (ctx->has_more_positionals(state)) {
        const positional_argument_t &positional = ctx->next_positional(state);
        const string_t &name = ctx->argv.at(positional.idx_in_argv);
        if (name.size() == range.length && this->range_equals_string(range, name)) {
            // The static argument matches
            state->argument_values[name].count += 1;
            ctx->acquire_next_positional(state);
            ctx->try_mark_fully_consumed(state);
            state_destructive_append_to(state, resulting_states);
        }
    } else {
        // No more positionals. Maybe suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state->suggested_next_arguments.insert(string_for_range(range));
        }
        // Append the state if we are allowing incomplete
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}

void match(const variable_clause_t &node, match_state_t *state, match_context_t *ctx, match_state_list_t *resulting_states) const {
    // Variable argument
    const range_t &range = node.word.range;
    if (ctx->has_more_positionals(state)) {
        // Note we retain the brackets <> in the variable name
        const string_t name = string_for_range(range);
        arg_t *arg = &state->argument_values[name];
        const positional_argument_t &positional = ctx->acquire_next_positional(state);
        arg->values.push_back(ctx->argv.at(positional.idx_in_argv));
        ctx->try_mark_fully_consumed(state);
        state_destructive_append_to(state, resulting_states);
    } else {
        // No more positionals. Suggest one.
        if (ctx->flags & flag_generate_suggestions) {
            state->suggested_next_arguments.insert(string_for_range(range));
        }
        if (ctx->flags & flag_match_allow_incomplete) {
            state_destructive_append_to(state, resulting_states);
        }
    }
}

option_map_t finalize_option_map(const option_map_t &map, const option_list_t &all_options, const range_list_t &all_variables, parse_flags_t flags) {
    // If we aren't asked to do empty args, then skip it
    if (! (flags & flag_generate_empty_args)) {
        return map;
    }
    
    // For each option, fill in the value in the map
    // This could be made more efficient via a single call to insert()
    option_map_t result = map;
    for (size_t i=0; i < all_options.size(); i++) {
        const option_t &opt = all_options.at(i);
        string_t name = opt.longest_name_as_string(this->source);
        // We merely invoke operator[]; this will do the insertion with a default value if necessary.
        // Note that this is somewhat nasty because it may unnecessarily copy the key. We might use a find() beforehand to save memory
        result[name];
        
        if (opt.has_value() && ! opt.default_value_range.empty()) {
            // Maybe pply the default value for the variable
            const string_t variable_name(this->source, opt.value.start, opt.value.length);
            arg_t *var_arg = &result[variable_name];
            if (var_arg->values.empty())
            {
                var_arg->values.push_back(string_for_range(opt.default_value_range));
            }
        }
    }
    
    // Fill in variables
    string_t name;
    for (size_t i=0; i < all_variables.size(); i++) {
        const range_t &var_range = all_variables.at(i);
        name.assign(this->source, var_range.start, var_range.length);
        // As above, we merely invoke operator[]
        result[name];
    }
    
    // Fill in static arguments
    for (size_t i=0; i < all_static_arguments.size(); i++) {
        const range_t &range = all_static_arguments.at(i);
        name.assign(this->source, range.start, range.length);
        // As above, we merely invoke operator[]
        result[name];
    }
    
    return result;
}

/* Matches argv */
option_map_t match_argv(const string_list_t &argv, parse_flags_t flags, const positional_argument_list_t &positionals, const resolved_option_list_t &resolved_options, const option_list_t &all_options, const range_list_t &all_variables, index_list_t *out_unused_arguments, bool log_stuff = false) {
    /* Set flag_stop_after_consuming_everything. This allows us to early-out. */
    match_context_t ctx(flags | flag_stop_after_consuming_everything, positionals, resolved_options, argv);
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
            for (typename option_map_t::const_iterator iter = state.argument_values.begin(); iter != state.argument_values.end(); ++iter) {
                const string_t &name = iter->first;
                const arg_t &arg = iter->second;
                fprintf(stderr, "\t%ls: ", widen(name).c_str());
                for (size_t j=0; j < arg.values.size(); j++) {
                    if (j > 0) {
                        fprintf(stderr, ", ");
                    }
                    fprintf(stderr, "%ls", widen(arg.values.at(j)).c_str());
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
        return finalize_option_map(result.at(best_state_idx).argument_values, all_options, all_variables, flags);
    } else {
        // No states. Every argument is unused.
        if (out_unused_arguments != NULL) {
            out_unused_arguments->clear();
            for (size_t i=0; i < argv.size(); i++) {
                out_unused_arguments->push_back(i);
            }
        }
        return finalize_option_map(option_map_t(), all_options, all_variables, flags);
    }
}

/* Parses the docopt, etc. Returns true on success, false on error */
bool preflight(error_list_t *out_errors) {
    // Populate our instance variables
    this->populate_by_walking_lines(out_errors);
    
    /* If we have no usage, apply the default one */
    if (this->usages.empty())
    {
        this->usages.resize(1);
        this->usages.back().make_default();
    }
    
    // Extract options and variables from the usage sections
    option_list_t usage_options;
    this->collect_options_and_variables(this->usages, &usage_options, &this->all_variables, &this->all_static_arguments);

    // Combine these into a single list
    this->all_options.reserve(usage_options.size() + this->shortcut_options.size());
    this->all_options.insert(this->all_options.end(), usage_options.begin(), usage_options.end());
    this->all_options.insert(this->all_options.end(), this->shortcut_options.begin(), this->shortcut_options.end());
    this->uniqueize_options(&this->all_options, false /* do not error on duplicates */, out_errors);

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
            if (options_have_same_name(shortcut_opt, usage_opt)) {
                // Remove this shortcut, and decrement the index to reflect the position shift of the remaining items
                this->shortcut_options.erase(this->shortcut_options.begin() + i);
                i-=1;
                break;
            }
        }
    }
    
    // Example of how to dump
    if (0)
    {
        std::string dumped;
        for (size_t i=0; i < this->usages.size(); i++)
        {
            dumped += node_dumper_t::dump_tree(this->usages.at(i), this->source);
        }
        fprintf(stderr, "%s\n", dumped.c_str());
    }
    
    /* Successfully preflighted */
    return true;
}

// TODO: make this const by stop touching error_list
option_map_t best_assignment_for_argv(const string_list_t &argv, parse_flags_t flags, error_list_t *out_errors, index_list_t *out_unused_arguments)
{
    positional_argument_list_t positionals;
    resolved_option_list_t resolved_options;
    
    // Extract positionals and arguments from argv
    this->separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals, &resolved_options, out_errors);
    
    // Produce an option map
    option_map_t result = this->match_argv(argv, flags, positionals, resolved_options, all_options, all_variables, out_unused_arguments);
    
    return result;
}

string_list_t suggest_next_argument(const string_list_t &argv, parse_flags_t flags) const
{
    /* Set internal flags to generate suggestions */
    flags |= flag_generate_suggestions;
    
    positional_argument_list_t positionals;
    resolved_option_list_t resolved_options;
    string_t suggestion;
    this->separate_argv_into_options_and_positionals(argv, all_options, flags, &positionals, &resolved_options, NULL /* errors */, &suggestion);
    
    /* If we got a suggestion, it means that the last argument was of the form --foo, where --foo wants a value. That's all we care about. */
    if (! suggestion.empty()) {
        return string_list_t(1, suggestion);
    }
    
    match_context_t ctx(flags, positionals, resolved_options, argv);
    match_state_t init_state;
    init_state.consumed_options.resize(resolved_options.size(), false);
    match_state_list_t states;
    match(this->usages, &init_state, &ctx, &states);
    
    /* Find the state(s) with the fewest unused arguments, and then insert all of their suggestions into a list */
    string_list_t all_suggestions;
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

string_t commands_for_variable(const string_t &var_name) const {
    string_t result;
    typename variable_command_map_t::const_iterator where = this->variables_to_commands.find(var_name);
    if (where != this->variables_to_commands.end()) {
        const range_t &cond_range = where->second;
        result.assign(this->source, cond_range.start, cond_range.length);
    }
    return result;
}

string_t description_for_option(const string_t &given_option_name) const {
    if (given_option_name.size() < 2 || given_option_name.at(0) != '-')
    {
        return string_t();
    }

    string_t result;
    const bool has_double_dash = (given_option_name.at(1) == '-');
    // We have to go through our options and compare their short_name or corresponding_long_name to the given string
    for (size_t i=0; i < this->all_options.size(); i++) {
        const option_t &opt = this->all_options.at(i);
        bool matches = false;
        
        // We can skip options without descriptions
        if (opt.description_range.empty()) {
            continue;
        }
        
        // Check short options
        if (opt.type == option_t::single_short || opt.type == option_t::single_long) {
            // The 1 skips the leading dash
            matches = this->range_equals_string(opt.name, given_option_name, 1);
        } else if (opt.type == option_t::double_long && has_double_dash) {
            matches = this->range_equals_string(opt.name, given_option_name, 2);
        }
        
        if (matches) {
            result.assign(this->source, opt.description_range.start, opt.description_range.length);
            break;
        }
    }
    return result;
}

std::vector<string_t> get_command_names() const {
    /* Get the command names. We store a set of seen names so we only return tha names once, but in the order matching their appearance in the usage spec. */
    std::vector<string_t> result;
    std::set<string_t> seen;
    for (size_t i=0; i < this->usages.size(); i++) {
        const usage_t &usage = this->usages.at(i);
        range_t name_range = usage.prog_name.range;
        if (! name_range.empty()) {
            const string_t name(this->source, name_range.start, name_range.length);
            if (seen.insert(name).second) {
                result.push_back(name);
            }
        }
    }
    return result;

}

std::vector<string_t> get_variables() const {
    std::vector<string_t> result;
    
    // Include explicit variables
    for (size_t i=0; i < this->all_variables.size(); i++) {
        const range_t &r = this->all_variables.at(i);
        result.push_back(string_for_range(r));
    }
    
    // Include variables that are part of options
    for (size_t i=0; i < this->all_options.size(); i++) {
        const range_t &r = this->all_options.at(i).value;
        if (! r.empty()) {
            result.push_back(string_for_range(r));
        }
    }
    
    // Sort and remove duplicates
    std::sort(result.begin(), result.end());
    result.erase(std::unique(result.begin(), result.end()), result.end());
    return result;
}

// close the class
CLOSE_DOCOPT_IMPL;

template<typename string_t>
std::vector<argument_status_t> argument_parser_t<string_t>::validate_arguments(const std::vector<string_t> &argv, parse_flags_t flags) const
{
    size_t arg_count = argv.size();
    std::vector<argument_status_t> result(arg_count, status_valid);

    index_list_t unused_args;
    impl->best_assignment_for_argv(argv, flags, NULL /* errors */, &unused_args);
    
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
    return impl->suggest_next_argument(argv, flags);
}

template<typename string_t>
string_t argument_parser_t<string_t>::commands_for_variable(const string_t &var) const
{
    return impl->commands_for_variable(var);
}

template<typename string_t>
string_t argument_parser_t<string_t>::description_for_option(const string_t &option) const
{
    return impl->description_for_option(option);
}

template<typename string_t>
std::vector<string_t> argument_parser_t<string_t>::get_command_names() const
{
    return impl->get_command_names();
}

template<typename string_t>
std::vector<string_t> argument_parser_t<string_t>::get_variables() const
{
    return impl->get_variables();
}

template<typename string_t>
std::map<string_t, base_argument_t<string_t> >
argument_parser_t<string_t>::parse_arguments(const std::vector<string_t> &argv,
                                            parse_flags_t flags,
                                            std::vector<error_t<string_t> > *out_errors,
                                            std::vector<size_t> *out_unused_arguments) const {
    return impl->best_assignment_for_argv(argv, flags, out_errors, out_unused_arguments);
}


template<typename string_t>
bool argument_parser_t<string_t>::set_doc(const string_t &doc, std::vector<error_t<string_t> > *out_errors) {
    docopt_impl<string_t> *new_impl = new docopt_impl<string_t>(doc);
    
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
        this->impl = new docopt_impl<string_t>(*rhs.impl);
    }
}

template<typename string_t>
argument_parser_t<string_t> &argument_parser_t<string_t>::operator=(const argument_parser_t &rhs) {
    if (this != &rhs) {
        delete this->impl;
        if (rhs.impl == NULL) {
            this->impl = NULL;
        } else {
            this->impl = new docopt_impl<string_t>(*rhs.impl);
        }
    }
    return *this;
}


/* Destructor */
template<typename string_t>
argument_parser_t<string_t>::~argument_parser_t<string_t>() {
    /* Clean up guts */
    delete impl; // may be null
}

// close the namespace
CLOSE_DOCOPT_IMPL

// Force template instantiation
template class docopt_fish::argument_parser_t<std::string>;
template class docopt_fish::argument_parser_t<std::wstring>;


