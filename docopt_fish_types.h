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
    
    /* If the receiver is empty, replace it with the new range */
    void replace_if_empty(const range_t &rhs) {
        if (this->empty()) {
            *this = rhs;
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
    
    // The types of names
    // This is also used indexes into an array
    enum name_type_t {
        single_short, // -f
        single_long, // -foo
        double_long, // --foo
        
        NAME_TYPE_COUNT
    };

    range_t names[NAME_TYPE_COUNT];

    // value of the option, i.e. variable name. Empty for no value.
    range_t value;
    
    // Range of the description. Empty for none.
    range_t description_range;
    
    // Range of the default value. Empty for none.
    range_t default_value_range;
    
    // How we separate the name from the value
    enum separator_t {
        sep_space, // curl -o file
        sep_equals, // -std=c++98
        sep_none // -DNDEBUG. Must be a single_short option.
    } separator;
    
    option_t() : separator(sep_space) {}
    
    option_t(enum name_type_t type, const range_t &name, const range_t &v, separator_t sep) : value(v), separator(sep) {
        assert(type < NAME_TYPE_COUNT);
        this->names[type] = name;
    }
    
    bool has_type(name_type_t type) const {
        assert(type < NAME_TYPE_COUNT);
        return ! this->names[type].empty();
    }

    name_type_t best_type() const {
        if (this->has_type(double_long)) return double_long;
        else if (this->has_type(single_long)) return single_long;
        return single_short;
    }

    // Returns the "best" (longest) name
    range_t best_name() const {
        return this->names[this->best_type()];
    }
    
    /* We want a value if we have a non-empty value range */
    bool has_value() const {
        return ! value.empty();
    }

    /* Hackish? Returns true if we share a name type */
    bool name_types_overlap(const option_t &rhs) const {
        bool result = false;
        size_t idx = NAME_TYPE_COUNT;
        while (idx--) {
            if (!this->names[idx].empty() && !rhs.names[idx].empty()) {
                result = true;
                break;
            }
        }
        return result;
    }
    
    // Returns true if the options have the same name, as determined by their respective ranges in src.
    template<typename string_t>
    bool has_same_name(const option_t &opt2, const string_t &src) const {
        bool result = false;
        size_t idx = NAME_TYPE_COUNT;
        while (idx-- && !result) {
            const range_t r1 = this->names[idx], r2 = opt2.names[idx];
            if (r1.length > 0 && r1.length == r2.length) {
                // Name lengths must be the same
                if (r1 == r2) {
                    // Identical ranges
                    result = true;
                } else {
                    result = (0 == src.compare(r1.start, r1.length, src, r2.start, r2.length));
                }
            }
        }
        return result;
    }

    /* Helper function for dumping */
    template<typename string_t>
    string_t describe(const string_t &src) const {
        string_t result;
        string_t tmp;
        range_t name = this->best_name();
        result.append(src, name.start, name.length);
        
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
    
    template<typename string_t>
    string_t name_as_string(name_type_t type, const string_t &src) const {
        assert(type < NAME_TYPE_COUNT);
        const unsigned dash_count = (type == double_long ? 2 : 1);
        const range_t name_range = this->names[type];
        assert(! name_range.empty());
        string_t result;
        result.reserve(dash_count + name_range.length);
        result.append(dash_count, '-');
        result.append(src, name_range.start, name_range.length);
        return result;
    }

    // Returns the "best" name, plucking it out of the given source. Includes dashes.
    template<typename string_t>
    string_t best_name_as_string(const string_t &src) const {
        return this->name_as_string(this->best_type(), src);
    }
    
    /* Acquire "guts" from another option wherever we have blanks */
    void merge_from(const option_t &rhs) {
        for (size_t i=0; i < NAME_TYPE_COUNT; i++) {
            this->names[i].replace_if_empty(rhs.names[i]);
        }
        // Ensure we copy over the separator type
        if (this->value.empty()) {
            this->separator = rhs.separator;
        }
        this->value.replace_if_empty(rhs.value);
        this->description_range.replace_if_empty(rhs.description_range);
        this->default_value_range.replace_if_empty(rhs.default_value_range);
    }
    
    /* Given a string and the inout range 'remaining', parse out an option and return it. Update the remaining range to reflect the number of characters used. */
    template<typename string_t>
    static bool parse_from_string(const string_t &str, range_t *remaining, option_t *result, std::vector<error_t<string_t> >* errors = NULL );

    /* Variant for when the remaining range is uninteresting. */
    template<typename string_t>
    static bool parse_from_string(const string_t &str, range_t range, option_t *result, std::vector<error_t<string_t> > *errors = NULL) {
        return parse_from_string(str, &range, result, errors);
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
