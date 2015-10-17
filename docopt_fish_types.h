#ifndef DOCOPT_FISH_TYPES_H
#define DOCOPT_FISH_TYPES_H

#include <vector>
#include <assert.h>
#include <cstring>
#include <stdio.h>

/* Hide open and close brackets to avoid an annoying leading indent inside our class */
#define OPEN_DOCOPT_IMPL {
#define CLOSE_DOCOPT_IMPL }

#define UNUSED __attribute__((unused))

namespace docopt_fish
OPEN_DOCOPT_IMPL


/* Overloads */
UNUSED
static inline void assign_narrow_string_to_string(const char *s, std::string *result) {
    *result = s;
}

UNUSED
static inline void assign_narrow_string_to_string(const char *s, std::wstring *result) {
    size_t len = std::strlen(s);
    for (size_t i=0; i < len; i++) {
        char c = s[i];
        assert(c >= 0 && (unsigned char)c <= 127); //ASCII only
        result->push_back(wchar_t(c));
    }
}

UNUSED
static inline const std::wstring &widen(const std::wstring &t) {
    return t;
}

UNUSED
static inline std::wstring widen(const std::string &t) {
    std::wstring result;
    result.insert(result.begin(), t.begin(), t.end());
    return result;
}


/* Class representing a range of a string */
struct range_t {
    size_t start;
    size_t length;
    range_t() : start(0), length(0) {}
    range_t(size_t s, size_t l) : start(s), length(l) {}
    
    /* Returns start + length, dying on overflow */
    size_t end() const {
        size_t result = start + length;
        assert(result >= start); //don't overflow
        return result;
    }
    
    /* Returns whether the range is empty */
    bool empty() const {
        return length == 0;
    }
    
    /* Equality and inequality */
    bool operator==(const range_t &rhs) const {
        return this->start == rhs.start && this->length == rhs.length;
    }

    bool operator!=(const range_t &rhs) const {
        return !(*this == rhs);
    }

    /* Merges a range into this range. After merging, the receiver is the smallest range containing every index that is in either range. Empty ranges are discarded. */
    void merge(const range_t &rhs) {
        if (this->empty()) {
            *this = rhs;
        } else if (! rhs.empty()) {
            this->start = std::min(this->start, rhs.start);
            this->length = std::max(this->end(), rhs.end()) - this->start;
        }
    }
};

typedef std::vector<range_t> range_list_t;


/* A token is just a range of some string, with a type */
struct token_t {
    range_t range;
    token_t(const range_t &r) : range(r) {}
    token_t() {}

    bool empty() const {
        return range.empty();
    }
};
typedef std::vector<token_t> token_list_t;

typedef std::vector<size_t> index_list_t;

/* An option represents something like '--foo=bar' */
struct option_t {
    enum type_t {
        single_short, // '-f'
        single_long, // '-foo'
        double_long // '--foo'
    } type;
    
    enum separator_t {
        sep_space, // curl -o file
        sep_equals, // -std=c++98
        sep_none // -DNDEBUG. Must be a single_short option.
    };

    // name of the option, like '--foo'
    range_t name;

    // value of the option, i.e. variable name. Empty for no value.
    range_t value;
    
    // key name of the option, e.g. if we have both -v and --verbose, this will be 'verbose' for the -v option. Empty for none.
    range_t corresponding_long_name;

    // Range of the description. Empty for none.
    range_t description_range;
    
    // Range of the default value. Empty for none.
    range_t default_value_range;

    // How we separate the name from the value. We may make this a bitmap some day.
    separator_t separator;

    option_t() {}
    
    option_t(const range_t &n, const range_t &v, size_t leading_dash_count, separator_t sep) : name(n), value(v), separator(sep) {
        // Set the type. If there is only one dash, we infer single_long and single_short by the length of the name
        if (leading_dash_count > 1) {
            this->type = option_t::double_long;
            this->corresponding_long_name = this->name;
        } else if (n.length > 1) {
            this->type = option_t::single_long;
        } else {
            this->type = option_t::single_short;
        }
    }

    /* We want a value if we have a non-empty value range */
    bool has_value() const {
        return ! value.empty();
    }

    // Returns true if the options have the same name, as determined by their respective ranges in src.
    template<typename string_t>
    bool has_same_name(const option_t &opt2, const string_t &src) const {
        bool result = false;
        if (this->name.length == opt2.name.length) {
            // Name lengths must be the same
            if (this->name == opt2.name) {
                // Identical ranges
                result = true;
            } else {
                result = (0 == src.compare(this->name.start, this->name.length, src, opt2.name.start, opt2.name.length));
            }
        }
        return result;
    }
    
    bool operator==(const option_t &rhs) const {
        return this->type == rhs.type &&
               this->separator == rhs.separator &&
               this->name == rhs.name &&
               this->value == rhs.value;
    }

    /* Helper function for dumping */
    template<typename string_t>
    string_t describe(const string_t &src) const {
        string_t result;
        string_t tmp;
        
        result.append(src, name.start, name.length);
        if (! corresponding_long_name.empty()){
            result.push_back('/');
            result.append(src, corresponding_long_name.start, corresponding_long_name.length);
        }

        if (! value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(src, value.start, value.length);
        }
        result.push_back(' ');
        
        char range[64];
        snprintf(range, sizeof range, "<%lu, %lu>", name.start, name.length);
        assign_narrow_string_to_string(range, &tmp);
        result.append(tmp);
        
        if (this->separator == sep_none) {
            assign_narrow_string_to_string(" (no sep)", &tmp);
            result.append(tmp);
        } else if (this->separator == sep_equals) {
            assign_narrow_string_to_string(" (= sep)", &tmp);
            result.append(tmp);
        }

        
        return result;
    }
    
    // Returns the "longest" name (using corresponding_long_name if possible), plucking it out of the given source. Includes dashes.
    template<typename string_t>
    string_t longest_name_as_string(const string_t &src) const {
        string_t result;
        // Everyone gets at least one dash; doubles get two
        bool use_long_name = ! this->corresponding_long_name.empty();
        result.push_back('-');
        if (use_long_name || this->type == double_long) {
            result.push_back('-');
        }
        const range_t &effective_range = use_long_name ? this->corresponding_long_name : this->name;
        result.append(src, effective_range.start, effective_range.length);
        return result;
    }
    
    // Returns the normal name (short or long), plucking it out of the given source. Includes dashes.
    template<typename string_t>
    string_t name_as_string(const string_t &src) const {
        string_t result;
        result.push_back('-');
        if (this->type == double_long) {
            result.push_back('-');
        }
        result.append(src, this->name.start, this->name.length);
        return result;
    }
    
    /* Given a string and the inout range 'remaining', parse out an option and return it. Update the remaining range to reflect the number of characters used. */
    template<typename string_t>
    static option_t parse_from_string(const string_t &str, range_t *remaining, std::vector<error_t<string_t> >* errors = NULL );

    /* Variant for when the remaining range is uninteresting. */
    template<typename string_t>
    static option_t parse_from_string(const string_t &str, range_t range, std::vector<error_t<string_t> > *errors = NULL) {
        return parse_from_string(str, &range, errors);
    }

};
typedef std::vector<option_t> option_list_t;

template <typename string_t>
static void append_error(std::vector<error_t<string_t> > *errors, size_t where, int code, const char *txt, size_t arg_idx = -1) {
    if (errors != NULL) {
        errors->resize(errors->size() + 1);
        error_t<string_t> *error = &errors->back();
        error->location = where;
        error->code = code;
        error->argument_index = arg_idx;
        assign_narrow_string_to_string(txt, &error->text);
    }
}

/* Internal flags */
enum {
    /* When matching, if we run out of positionals or options, instead of failing, return a match containing a suggestion */
    flag_generate_suggestions = 1U << 16,
    
    /* When matching, if we consume all positionals and options, stop searching. */
    flag_stop_after_consuming_everything = 1U << 17
};

/* Error codes */
enum {
    error_none,
    
    /* Errors that may occur in a docopt description */
    error_excessive_dashes, // Three or more dashes in an option: prog ---foo
    error_leading_ellipsis, // ... appearing without an associated expression
    error_excessive_equal_signs, // Two or more equal signs: --foo==bar
    error_bad_option_separator, // Bad separator between option and value: --foo<bar>
    error_invalid_option_name, // Bad option name: Options: foo
    error_invalid_variable_name, // Bad variable name: Options: --foo=
    error_missing_close_paren, // Missing ): prog (foo
    error_missing_close_bracket, // Missing ]:  prog [foo
    error_missing_close_bracket_in_default, // No close bracket. Options: --send <msg> Specifies message [default: none
    error_missing_close_variable, // Missing >: <foo
    error_one_variable_multiple_commands, // Two commands for same var. <msg> foo \n <msg> foo
    error_option_duplicated_in_options_section, // Options: --foo, --foo
    error_trailing_vertical_bar, // Usage: prog foo | bar |
    error_unknown_leader, // Unknown leader on a line, e.g. leading ;
    
    // Errors that may occur in arguments (argv)
    // Lower values are more "likely" errors
    error_option_has_missing_argument = 100, // Option expects an argument, but none was given in argv
    error_option_unexpected_argument, // Option does not expect an argument, but one was given in argv
    error_ambiguous_prefix_match, // Prefix matching was requested and the result was ambiguous
    error_unknown_option, // Option is not present in usage
    error_wrong_separator // Wrong sort of separator
};

CLOSE_DOCOPT_IMPL

#endif
