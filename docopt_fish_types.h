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

/* Oh god, we have our own string type? Why?
 
 This grew out of a desire to support both char and wchar_t strings. Originally this was done with pervasive use of templates, but it became insane. So the first thing that rstring_t does is abstract away character widths, without using templates.
 
 A second requirement is to track offsets, so that (e.g.) when we find a parse error, we know the location in the docopt spec and can return that information. Originally this was done by passing around ranges everywhere, and being careful to keep track of the original string from which the ranges came, and this was factored out of that: we have a base pointer and a range.
 
 A third requirement is to have excellent performance with low memory usage. rstring_t is a small value type which does not allocate memory.
 
 The biggest risk of rstring_t is lifetime control. The base pointer is not managed, so nothing prevents you from creating an rstring_t pointing at a std::string and then deallocating that string. docopt does not have long-running processes so it is generally pretty easy to reason about lifetimes, but be careful to ensure that rstring_ts do not outlive their source.
 
 Note that rstring_ts are not null terminated.
*/
class rstring_t {
public:
    typedef uint32_t char_t;

private:
    size_t start_;
    size_t length_;
    const void *base_;
    // note you can use shift to multiply by a width
    enum width_t {
        width1 = 0,
        width2 = 1,
        width4 = 2
    } width_;

    rstring_t(const void *base, size_t start, size_t length, width_t width) : start_(start), length_(length), base_(base), width_(width) {}
    
    explicit rstring_t(const char_t *b, size_t start, size_t length) : start_(start), length_(length), base_(b), width_(resolve_width<char_t>()) {}
    
    width_t width() const {
        return this->width_;
    }
    
    // Avoid bad sign extension
    static inline char_t to_char(char c) {
        return static_cast<unsigned char>(c);
    }
    
    template<typename T>
    const T *base_as() const {
        return reinterpret_cast<const T*>(this->base_);
    }
    
    template<typename T>
    const T *ptr_begin() const {
        const uint8_t *p = this->base_as<uint8_t>();
        p += this->start_ * sizeof(T);
        return reinterpret_cast<const T *>(p);
    }
    
    template<typename T, bool case_insensitive>
    size_t find_internal(const char *needle) const {
        // Empty string always found first
        if (needle[0] == '\0') {
            return 0;
        }
        const char_t his_first_low = (case_insensitive ? tolower(needle[0]) : to_char(needle[0]));
        const char_t his_first_up = (case_insensitive ? toupper(needle[0]) : to_char(needle[0]));
        
        const T *haystack = this->ptr_begin<T>();
        size_t haystack_count = this->length();
        for (size_t outer=0; outer < haystack_count; outer++) {
            // Quick check for first character
            if (haystack[outer] != his_first_low && haystack[outer] != his_first_up) {
                continue;
            }
            
            // Ok, we know the first character matches
            // See if there's a match at 'outer'
            for (size_t inner = 1;;inner++) {
                if (needle[inner] == '\0') {
                    // we exhausted the needle, so we have a match at 'outer'
                    return outer;
                } else if (outer + inner >= haystack_count) {
                    // ran off our end
                    // this means that whatever's left is shorter than needle,
                    // so we're done
                    return npos;
                } else {
                    char_t mine = haystack[outer + inner];
                    if (mine >= 256) {
                        // outside of ASCII, no match possible
                        break;
                    }
                    char_t his = to_char(needle[inner]);
                    bool matches = (mine == his || (case_insensitive && tolower(mine) == tolower(his)));
                    if (! matches) {
                        // no match at 'outer'
                        break;
                    }
                }
            }
        }
        return npos;
    }
    
    template<typename T1, typename T2>
    static int compare_internal2(const rstring_t &lhs, const rstring_t &rhs) {
        size_t len1 = lhs.length(), len2 = rhs.length();
        size_t amt = std::min(len1, len2);
        const T1 *p1 = lhs.ptr_begin<T1>();
        const T2 *p2 = rhs.ptr_begin<T2>();
        for (size_t i=0; i < amt; i++) {
            char_t c1 = p1[i], c2 = p2[i];
            if (c1 != c2) {
                return c1 < c2 ? -1 : 1;
            }
        }
        if (len1 != len2) {
            return len1 < len2 ? -1 : 1;
        }
        return 0;
    }
    
    template<typename T1>
    int compare_internal1(const rstring_t &rhs) const {
        switch (rhs.width()) {
            case width1:
                return compare_internal2<T1, uint8_t>(*this, rhs);
            case width2:
                return compare_internal2<T1, uint16_t>(*this, rhs);
            case width4:
                return compare_internal2<T1, uint32_t>(*this, rhs);
        }
    }
    
    template<typename T>
    size_t find_1_internal(char_t needle) const {
        size_t len = this->length();
        const T *haystack = this->ptr_begin<T>();
        for (size_t i=0; i < len; i++) {
            if (haystack[i] == needle) {
                return i;
            }
        }
        return npos;
    }
    
    typedef bool (*scan_predicate_t)(char_t);
    
    template<typename T, scan_predicate_t F>
    rstring_t scan_while_internal() {
        const size_t length = this->length();
        const T *haystack = this->ptr_begin<T>();
        size_t amt = 0;
        while (amt < length && F(haystack[amt])) {
            amt++;
        }
        rstring_t result = this->substr(0, amt);
        this->start_ += amt;
        this->length_ -= amt;
        return result;
    }
    
public:
    
    static const size_t npos = size_t(-1);

    size_t start() const {
        return this->start_;
    }

    size_t end() const {
        assert(this->start_ + this->length_ >= this->start_);
        return this->start_ + this->length_;
    }
    
    size_t length() const {
        return this->length_;
    }
    
    bool empty() const {
        return this->length_ == 0;
    }
    
    char_t at(size_t idx) const {
        assert(idx <= length_);
        size_t offset = idx + this->start_;
        switch (this->width()) {
            case width1:
                return this->base_as<uint8_t>()[offset];
            case width2:
                return this->base_as<uint16_t>()[offset];
            case width4:
                return this->base_as<uint32_t>()[offset];
        }
    }
    
    rstring_t substr(size_t offset, size_t length) const {
        assert(offset + length >= offset && offset + length <= this->length());
        return rstring_t(this->base_, this->start_ + offset, length, this->width_);
    }
    
    // Finds needle in self, and returns the location or npos
    size_t find(const char *needle) const {
        switch (this->width()) {
            case width1:
                return this->find_internal<uint8_t, false>(needle);
            case width2:
                return this->find_internal<uint16_t, false>(needle);
            case width4:
                return this->find_internal<uint32_t, false>(needle);
        }
    }
    
    size_t find_case_insensitive(const char *needle) const {
        switch (this->width()) {
            case width1:
                return this->find_internal<uint8_t, true>(needle);
            case width2:
                return this->find_internal<uint16_t, true>(needle);
            case width4:
                return this->find_internal<uint32_t, true>(needle);
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
        switch (this->width()) {
            case width1:
                return this->find_1_internal<uint8_t>(needle);
            case width2:
                return this->find_1_internal<uint16_t>(needle);
            case width4:
                return this->find_1_internal<uint32_t>(needle);
        }
    }

    rstring_t substr_from(size_t offset) const {
        assert(offset <= this->length());
        return this->substr(offset, this->length() - offset);
    }
    
    const void *base() const {
        return this->base_;
    }
    
    /* Merges another string into this string. If both strings are nonempty, they must have the same base pointer and width. */
    rstring_t merge(const rstring_t &rhs) const {
        if (this->empty()) {
            return rhs;
        } else if (rhs.empty()) {
            return *this;
        } else {
            assert(this->base_ == rhs.base_ && this->width_ == rhs.width_);
            size_t start = std::min(this->start_, rhs.start_);
            size_t length = std::max(this->end(), rhs.end()) - start;
            return rstring_t(this->base_, start, length, this->width_);
        }
    }
    
    void replace_if_empty(const rstring_t &rhs) {
        if (this->empty()) {
            *this = rhs;
        }
    }
    
    int compare(const rstring_t &rhs) const {
        if (this == &rhs) {
            return 0;
        }
        if (this->base_ == rhs.base_ && this->start_ == rhs.start_ && this->length_ == rhs.length_) {
            return 0;
        }
        
        switch (this->width()) {
            case width1:
                return this->compare_internal1<uint8_t>(rhs);
            case width2:
                return this->compare_internal1<uint16_t>(rhs);
            case width4:
                return this->compare_internal1<uint32_t>(rhs);
        }
    }
    
    bool operator==(const rstring_t &rhs) const {
        return this->length() == rhs.length() && this->compare(rhs) == 0;
    }
    
    bool operator!=(const rstring_t &rhs) const {
        return ! (*this == rhs);
    }
    
    bool operator<(const rstring_t &rhs) const {
        return this->compare(rhs) < 0;
    }
    
    char_t operator[](size_t idx) const {
        return this->at(idx);
    }
    
    // Can compare against const char *
    // These are always statically known, so the strlen is free
    bool has_prefix(const char *s) const {
        size_t len = strlen(s);
        if (len > this->length()) {
            return false;
        }
        for (size_t i=0; i < len; i++) {
            char_t si = to_char(s[i]);
            if (this->at(i) != si) {
                return false;
            }
        }
        return true;
    }
    
    bool is_double_dash() const {
        return this->length() == 2 && this->at(0) == '-' && this->at(1) == '-';
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
    template<scan_predicate_t F>
    rstring_t scan_while() {
        switch (this->width()) {
            case width1:
                return this->scan_while_internal<uint8_t, F>();
            case width2:
                return this->scan_while_internal<uint16_t, F>();
            case width4:
                return this->scan_while_internal<uint32_t, F>();
        }
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
                char_t ci = to_char(c[i]);
                if (this->at(i) != ci) {
                    break;
                }
            }
            if (i == len) {
                // Prefix matches prefix
                result = this->substr(0, len);
                this->start_ += len;
                this->length_ -= len;
            }
        }
        return result;
    }

    
    // If this begins with c, returns a string containing c
    // and adjusts self to the remainder. Otherwise returns
    // an empty string
    rstring_t scan_1_char(char c) {
        rstring_t result;
        if (this->length() > 0 && this->at(0) == to_char(c)) {
            result = this->substr(0, 1);
            this->start_ += 1;
            this->length_ -= 1;
        }
        return result;
    }
    
    static bool char_is_whitespace(rstring_t::char_t c) {
        switch (c) {
            case '\t':
            case '\n':
            case '\r':
            case ' ':
                return true;
            default:
                return false;
        }
    }
    
    // Returns a new string with leading and trailing whitespace trimmed
    rstring_t trim_whitespace() const {
        size_t left = 0, right = this->length();
        while (left < right && char_is_whitespace(this->at(left))) {
            left++;
        }
        while (right > left && char_is_whitespace(this->at(right - 1))) {
            right--;
        }
        assert(left <= right);
        return this->substr(left, right - left);
    }

    explicit rstring_t() : start_(0), length_(0), base_(NULL), width_(width1) {}
    
    // Constructor from std::string. Note this borrows the storage so we must not outlive it.
    template<typename stdchar_t>
    explicit rstring_t(const std::basic_string<stdchar_t> &b) : start_(0), length_(b.length()), base_(b.c_str()), width_(resolve_width<stdchar_t>()) {}
    
    explicit rstring_t(const char *s, size_t len) : start_(0), length_(len), base_(s), width_(width1) {}
};


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
        assert(name.length() >= 2);
        assert(name[0] == '-');
        if (type == double_long) {
            assert(name.length() >= 3);
            assert(name[1] == '-');
        }
        this->names[type] = name;
    }
    
    bool has_type(name_type_t type) const {
        assert(type < NAME_TYPE_COUNT);
        return ! this->names[type].empty();
    }
    
    // Returns the "best" (longest) name
    const rstring_t &best_name() const {
        size_t idx = NAME_TYPE_COUNT;
        while (idx--) {
            const rstring_t &name = this->names[idx];
            if (! name.empty()) {
                return name;
            }
        }
        return this->names[single_short]; // is empty
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
    std::string describe() const {
        std::string result;
        rstring_t name = this->best_name();
        result.append(name.std_string<std::string>());
        
        if (! value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(value.std_string<std::string>());
        }
        result.push_back(' ');
        
        char range[64];
        snprintf(range, sizeof range, "<%lu, %lu>", name.start(), name.length());
        result.append(range);
        
        if (this->separator == sep_none) {
            result += " (no sep)";
        } else if (this->separator == sep_equals) {
            result += " (= sep)";
        }

        return result;
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
    static bool parse_from_string(rstring_t *remaining, option_t *result, std::vector<error_t> *errors = NULL);

    /* Variant for when the remaining range is uninteresting. */
    static bool parse_from_string(rstring_t str, option_t *result, std::vector<error_t> *errors = NULL) {
        return parse_from_string(&str, result, errors);
    }
    
    /* Given an argument (i.e. from argv), produce an option. */
    static option_t parse_from_argument(const rstring_t &str, option_t::name_type_t type);

};
typedef std::vector<option_t> option_list_t;

inline void append_error(std::vector<error_t> *errors, size_t where, int code, const char *txt, size_t arg_idx = -1) {
    if (errors != NULL) {
        errors->resize(errors->size() + 1);
        error_t *error = &errors->back();
        error->location = where;
        error->code = code;
        error->argument_index = arg_idx;
        error->text = txt;
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
