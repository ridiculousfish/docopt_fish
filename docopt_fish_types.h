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

/* Our "rstring" string type tracks a range and base pointer. This enables both efficient sharing (fewer copies) and precise error messages, since we know the location of the error. CHAR is suggested to be either char or wchar_t. rstrings are views on top of immutable underlying data. Note that these are not null terminated. */
class rstring_t {
public:
    typedef uint32_t char_t;

private:
    range_t range_;
    const void *base_;
    // note you can use shift to multiply by a width
    enum width_t {
        width1 = 0,
        width2 = 1,
        width4 = 2
    } width_;

    rstring_t(const void *base, range_t range, width_t width) : range_(range), base_(base), width_(width) {}
    
    width_t width() const {
        return this->width_;
    }
    
    template<typename T>
    const T *base_as() const {
        return reinterpret_cast<const T*>(this->base_);
    }
    
    template<typename T>
    const T *ptr_begin() const {
        const uint8_t *p = this->base_as<uint8_t>();
        p += (this->range_.start << this->width());
        return reinterpret_cast<const T *>(p);
    }
    
    template<typename T>
    const T *ptr_end() const {
        const uint8_t *p = this->base_as<uint8_t>();
        p += (this->range_.end() << this->width());
        return reinterpret_cast<const T *>(p);
    }

    
    template<typename T>
    size_t find_internal(const char *needle) const {
        // use uint to avoid sign extension crap
        const uint8_t *needle_start = reinterpret_cast<const uint8_t *>(needle);
        const uint8_t *needle_end = reinterpret_cast<const uint8_t *>(needle + strlen(needle));
        
        const T *haystack_start = this->ptr_begin<T>();
        const T *haystack_end = this->ptr_end<T>();
        const T *where = std::search(haystack_start, haystack_end, needle_start, needle_end);
        assert(where <= haystack_end);
        return where == haystack_end ? npos : where - haystack_start;
    }
    
    static inline unsigned int unreachable() {
#if defined(__clang__) || defined(__GNUC__)
        __builtin_unreachable();
#endif
        assert(false && "Unreachable code reached");
        return 0;
    }
    
public:
    
    static const size_t npos = size_t(-1);
    
    size_t length() const {
        return this->range_.length;
    }
    
    bool empty() const {
        return this->range_.empty();
    }
    
    char_t at(size_t idx) const {
        assert(idx <= range_.length);
        size_t offset = idx + this->range_.start;
        switch (this->width()) {
            case width1:
                return this->base_as<uint8_t>()[offset];
            case width2:
                return this->base_as<uint16_t>()[offset];
            case width4:
                return this->base_as<uint32_t>()[offset];
            default:
                return unreachable();
        }
    }
    
    int compare(const rstring_t &rhs) const {
        if (this == &rhs) {
            return 0;
        }
        if (this->base_ == rhs.base_ && this->range_ == rhs.range_) {
            return 0;
        }
        for (size_t i=0; i < this->length() && i < rhs.length(); i++) {
            if (this->at(i) != rhs.at(i)) {
                return this->at(i) < rhs.at(i) ? -1 : 1;
            }
        }
        // Data is equal, compare ranges
        if (this->length() != rhs.length()) {
            return this->length() < rhs.length() ? -1 : 1;
        }
        return 0;
    }
    
    rstring_t substr(const range_t &r) const {
        assert(r.end() <= this->length());
        return rstring_t(this->base_, range_t(this->range_.start + r.start, r.length), this->width_);
    }
    
    // Finds needle in self, and returns the location or npos
    size_t find(const char *needle) const {
        switch (this->width()) {
            case width1:
                return this->find_internal<uint8_t>(needle);
            case width2:
                return this->find_internal<uint16_t>(needle);
            case width4:
                return this->find_internal<uint32_t>(needle);
            default:
                unreachable();
                return 0;
        }
    }
    
    template<typename T>
    static width_t resolve_width() {
        switch (sizeof(T)) {
            case 1: return width1;
            case 2: return width2;
            case 4: return width4;
            default:
                assert(false && "Invalid width");
        }
    }
    
    size_t find(char_t needle) const {
        for (size_t i=0; i < this->length(); i++) {
            if (this->at(i) == needle) {
                return i;
            }
        }
        return npos;
    }

    rstring_t substr(size_t offset, size_t length) const {
        return this->substr(range_t(offset, length));
    }

    rstring_t substr(size_t offset) const {
        assert(offset <= this->length());
        return this->substr(offset, this->length() - offset);
    }
    
    range_t range() const {
        return this->range_;
    }
    
    /* Merges another string into this string. If both strings are nonempty, they must have the same base pointer and width. */
    rstring_t merge(const rstring_t &rhs) const {
        if (this->empty()) {
            return rhs;
        } else if (rhs.empty()) {
            return *this;
        } else {
            assert(this->base_ == rhs.base_ && this->width_ == rhs.width_);
            range_t merged = this->range_;
            merged.merge(rhs.range());
            return rstring_t(this->base_, merged, this->width_);
        }
    }
    
    void replace_if_empty(const rstring_t &rhs) {
        if (this->empty()) {
            *this = rhs;
        }
    }

    bool operator==(const rstring_t &rhs) const {
        return this->compare(rhs) == 0;
    }
    
    bool operator!=(const rstring_t &rhs) const {
        return this->compare(rhs) != 0;
    }
    
    bool operator<(const rstring_t &rhs) const {
        return this->compare(rhs) < 0;
    }
    
    char_t operator[](size_t idx) const {
        return this->at(idx);
    }

    // Copies our contents into the given std::string
    template<typename stdstring_t>
    void copy_to(stdstring_t *outstr) const {
        const size_t length = this->length();
        outstr->resize(length);
        for (size_t i=0; i < length; i++) {
            (*outstr)[i] = this->at(i);
        }
    }
    
    template<typename stdstring_t>
    const stdstring_t std_string() const {
        stdstring_t result;
        this->copy_to(&result);
        return result;
    }

    // Parsing stuff
    
    // Returns a prefix of self that satisfies the function.
    // Adjusts self to be the remainder after the prefix.
    template<typename F>
    rstring_t scan_while(F func) {
        size_t amt = 0;
        while (amt < this->length() && func(this->at(amt))) {
            amt++;
        }
        rstring_t result = this->substr(0, amt);
        *this = this->substr(amt);
        return result;
    }

    // If this begins with c, returns a string containing c
    // and adjusts self to the remainder. Otherwise returns
    // an empty string.
    rstring_t scan_string(const char *c) {
        rstring_t result;
        size_t len = strlen(c);
        if (len <= this->length()) {
            size_t i = 0;
            for (i=0; i < len; i++) {
                if (this->at(i) != c[i]) {
                    break;
                }
            }
            if (i == len) {
                // Prefix matches prefix
                result = this->substr(0, len);
                *this = this->substr(len);
            }
        }
        return result;
    }

    
    // If this begins with c, returns a string containing c
    // and adjusts self to the remainder. Otherwise returns
    // an empty string
    rstring_t scan_1_char(char_t c) {
        rstring_t result;
        if (this->length() > 0 && this->at(0) == c) {
            result = this->substr(0, 1);
            *this = this->substr(1);
        }
        return result;
    }
    
    // Returns a new string with leading and trailing whitespace trimmed
    rstring_t trim_whitespace() const {
        size_t left = 0, right = this->length();
        while (left < right && isspace(this->at(left))) {
            left++;
        }
        while (right > left && isspace(this->at(right - 1))) {
            right--;
        }
        assert(left <= right);
        return this->substr(left, right - left);
    }

    explicit rstring_t() : base_(NULL), range_(0, 0), width_(width1) {}
    explicit rstring_t(const char_t *b, const range_t &r) : base_(b), range_(r) {}
    
    // Constructor from std::string. Note this borrows the storage so we must not outlive it.
    template<typename stdchar_t>
    explicit rstring_t(const std::basic_string<stdchar_t> &b) : range_(0, b.length()), base_(b.c_str()), width_(resolve_width<stdchar_t>()) {}

    template<typename stdchar_t>
    explicit rstring_t(const std::basic_string<stdchar_t> &b, const range_t &r) : range_(r), base_(b.c_str()), width_(resolve_width<stdchar_t>()) {}
};

/* Overloads */

#warning why?
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

    rstring_t names[NAME_TYPE_COUNT];

    // value of the option, i.e. variable name. Empty for no value.
    rstring_t value;
    
    // Description. Empty for none.
    rstring_t description;
    
    // Default value. Empty for none.
    rstring_t default_value;
    
    // How we separate the name from the value
    enum separator_t {
        sep_space, // curl -o file
        sep_equals, // -std=c++98
        sep_none // -DNDEBUG. Must be a single_short option.
    } separator;
    
    option_t() : separator(sep_space) {}
    
    option_t(enum name_type_t type, const rstring_t &name, const rstring_t &v, separator_t sep) : value(v), separator(sep) {
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
    rstring_t best_name() const {
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
    
    // Returns true if the options have the same name
    bool has_same_name(const option_t &opt2) const {
        bool result = false;
        for (size_t i=0; i < NAME_TYPE_COUNT; i++) {
            const rstring_t &s1 = this->names[i], &s2 = opt2.names[i];
            if (s1.length() > 0 && s1 == s2) {
                result = true;
                break;
            }
        }
        return result;
    }

    /* Helper function for dumping */
    template<typename string_t>
    string_t describe() const {
        string_t result;
        string_t tmp;
        rstring_t name = this->best_name();
        result.append(name.std_string<string_t>());
        
        if (! value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(value.std_string<string_t>());
        }
        result.push_back(' ');
        
        char range[64];
        snprintf(range, sizeof range, "<%lu, %lu>", name.range().start, name.range().length);
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
    string_t name_as_string(name_type_t type) const {
        assert(type < NAME_TYPE_COUNT);
        const unsigned dash_count = (type == double_long ? 2 : 1);
        const rstring_t name = this->names[type];
        assert(! name.empty());
        string_t result;
        result.reserve(dash_count + name.length());
        name.copy_to(&result);
        result.insert(0, dash_count, '-');
        return result;
    }

    // Returns the "best" name, plucking it out of the given source. Includes dashes.
    template<typename string_t>
    string_t best_name_as_string() const {
        return this->name_as_string<string_t>(this->best_type());
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
        this->description.replace_if_empty(rhs.description);
        this->default_value.replace_if_empty(rhs.default_value);
    }
    
    /* Given a string and the inout range 'remaining', parse out an option and return it. Update the remaining range to reflect the number of characters used. */
    template<typename string_t>
    static bool parse_from_string(rstring_t *remaining, option_t *result, std::vector<error_t<string_t> >* errors = NULL);

    /* Variant for when the remaining range is uninteresting. */
    template<typename string_t>
    static bool parse_from_string(rstring_t str, option_t *result, std::vector<error_t<string_t> > *errors = NULL) {
        return parse_from_string(&str, result, errors);
    }

};
typedef std::vector<option_t> option_list_t;

#warning needs rstring
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
