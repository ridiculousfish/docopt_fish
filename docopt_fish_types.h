#ifndef DOCOPT_FISH_TYPES_H
#define DOCOPT_FISH_TYPES_H

#include <algorithm>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <cstring>
#include <climits>
#include <vector>
#include <array>
#include <set>

// Hide open and close brackets to avoid an annoying leading indent inside our class
#define OPEN_DOCOPT_IMPL namespace docopt_fish {
#define CLOSE_DOCOPT_IMPL }

#define UNUSED __attribute__((unused))

#if DOCOPT_USE_WCHAR
#define STRCONSTANT(x) L##x
#else
#define STRCONSTANT(x) x
#endif

OPEN_DOCOPT_IMPL

// rstring_t is a lightweight range of a base string
// It is just a base, start, and length
// Note that lifetimes are not controlled
// It is the user's responsibility to ensure that the rstring
// does not outlive the base pointer
class rstring_t {
   public:
    typedef string_t::value_type char_t;

   private:
    size_t start_;
    size_t length_;
    const char_t *base_;

    explicit rstring_t(const char_t *b, size_t start, size_t length)
        : start_(start), length_(length), base_(b) {}
    

    template<bool case_insensitive>
    inline size_t find_internal(const char *needle) const {
        // Empty string always found first
        if (needle[0] == '\0') {
            return 0;
        }
        
        auto does_match = [](char_t a, char b){
            if (static_cast<long>(a) >= CHAR_MAX) {
                // Outside of ASCII, no match possible
                return false;
            }
            return a == b || (case_insensitive && tolower(a) == tolower(b));
        };
        
        const char_t *result = std::search(this->begin(), this->end(),
                                           needle, needle + strlen(needle),
                                           does_match);
        return result == this->end() ? npos : result - this->begin();
    }

    static int compare_internal(const rstring_t &lhs, const rstring_t &rhs) {
        size_t len1 = lhs.length(), len2 = rhs.length();
        size_t amt = std::min(len1, len2);
        const char_t *lhs_end = lhs.begin() + amt;
        auto diffs = std::mismatch(lhs.begin(), lhs_end, rhs.begin());
        if (diffs.first != lhs_end) {
            // Some character differed
            return *diffs.first < *diffs.second ? -1 : 1;
        } else if (len1 != len2) {
            // No character differed, but lengths are different
            return len1 < len2 ? -1 : 1;
        } else {
            return 0;
        }
    }
    
    // Returns a substring of self up to length,
    // adjusting self to the remainder
    rstring_t chop_length(size_t len) {
        assert(len <= this->length());
        rstring_t result = this->substr(0, len);
        this->start_ += len;
        this->length_ -= len;
        return result;
    }

    typedef bool (*scan_predicate_t)(char_t);

   public:
    static const size_t npos = size_t(-1);

    size_t offset() const {
        return this->start_;
    }
    
    size_t end_offset() const {
        assert(this->start_ + this->length_ >= this->start_);
        return this->start_ + this->length_;
    }

    size_t length() const {
        return this->length_;
    }

    bool empty() const {
        return this->length_ == 0;
    }

    // note: we don't return references, since rstring_ts
    // are immutable
    char_t at(size_t idx) const {
        assert(idx <= this->length_);
        return this->base_[this->start_ + idx];
    }
    
    char_t front() const {
        assert(! this->empty());
        return this->at(0);
    }
    
    char_t back() const {
        assert(! this->empty());
        return this->at(this->length() - 1);
    }

    rstring_t substr(size_t offset, size_t length) const {
        assert(offset + length >= offset && offset + length <= this->length());
        return rstring_t(this->base_, this->start_ + offset, length);
    }

    // Finds needle in self, and returns the location or npos
    size_t find(const char *needle) const {
        return this->find_internal<false>(needle);
    }

    size_t find_case_insensitive(const char *needle) const {
        return this->find_internal<true>(needle);
    }

    rstring_t substr_from(size_t offset) const {
        assert(offset <= this->length());
        return this->substr(offset, this->length() - offset);
    }

    const void *base() const {
        return this->base_;
    }

    /* Merges another string into this string. If both strings are nonempty, they
     * must have the same
     * base pointer and width. */
    rstring_t merge(const rstring_t &rhs) const {
        if (this->empty()) {
            return rhs;
        } else if (rhs.empty()) {
            return *this;
        } else {
            assert(this->base_ == rhs.base_);
            size_t start = std::min(this->start_, rhs.start_);
            size_t end = std::max(this->start_ + this->length_, rhs.start_ + rhs.length_);
            return rstring_t(this->base_, start, end - start);
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
        if (this->base_ == rhs.base_ && this->start_ == rhs.start_ &&
            this->length_ == rhs.length_) {
            return 0;
        }

        return compare_internal(*this, rhs);
    }

    bool operator==(const rstring_t &rhs) const {
        return this->length() == rhs.length() && this->compare(rhs) == 0;
    }

    bool operator!=(const rstring_t &rhs) const {
        return !(*this == rhs);
    }

    bool operator<(const rstring_t &rhs) const {
        return this->compare(rhs) < 0;
    }

    char_t operator[](size_t idx) const {
        return this->at(idx);
    }

    // Can compare against const char *
    // These are often statically known, so the strlen is free
    inline bool has_prefix(const char *s) const {
        size_t len = strlen(s);
        if (len > this->length()) {
            return false;
        }
        return std::equal(s, s + len, this->begin());
    }

    bool is_double_dash() const {
        return this->length() == 2 && this->at(0) == '-' && this->at(1) == '-';
    }

    // Copies our contents into the given std::string
    void copy_to(string_t *outstr) const {
        const size_t length = this->length();
        outstr->resize(length);
        std::copy(this->begin(), this->end(), outstr->begin());
    }

    string_t std_string() const {
        string_t result;
        this->copy_to(&result);
        return result;
    }

    // Parsing stuff

    // Returns a prefix of self that satisfies the function.
    // Adjusts self to be the remainder after the prefix.
    template <scan_predicate_t F>
    rstring_t scan_while() {
        const char_t *prefix_end = std::find_if_not(this->begin(), this->end(), F);
        return this->chop_length(prefix_end - this->begin());
    }

private:
    // Stupid C++11 nonsense
    template<scan_predicate_t Unused>
    struct rstring_return_t { typedef rstring_t type; };

    template<int>
    std::tuple<> scan_multiple_helper() { return std::tuple<>(); }

    template<int, scan_predicate_t F, scan_predicate_t... Fs>
    std::tuple<rstring_t, typename rstring_return_t<Fs>::type...>
    scan_multiple_helper() {
        rstring_t fst = this->scan_while<F>();
        return std::tuple_cat(std::make_tuple(fst),
                              scan_multiple_helper<0, Fs...>());
    }

public:

    // Scan-while a sequence of predicates, in order
    template <scan_predicate_t... Fs>
    std::tuple<typename rstring_return_t<Fs>::type...>
    scan_multiple() {
        // we need to scan our arguments from left to right
        // list-initialization is supposed to guarantee left to right order,
        // but gcc bug 51253 means it sometimes won't
        // so we have to hack it with this obnoxious recursive implementation
        return scan_multiple_helper<0, Fs...>();
    }

    // If this begins with c, returns a string containing c
    // and adjusts self to the remainder. Otherwise returns
    // an empty string.
    rstring_t scan_string(const char *c) {
        // Here we hope the compiler can effectively optimize strlen(),
        // since it will be called again within has_prefix
        rstring_t result;
        if (this->has_prefix(c)) {
            result = this->chop_length(strlen(c));
        }
        return result;
    }

    // If this begins with c, returns a string containing c
    // and adjusts self to the remainder. Otherwise returns
    // an empty string
    rstring_t scan_1_char(char c) {
        rstring_t result;
        if (this->length() > 0 && this->at(0) == c) {
            result = this->chop_length(1);
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

    explicit rstring_t() : start_(0), length_(0), base_(NULL) {}
    
    // Iterator junk
    // Note rstring_ts do not allow mutating their contents
    // Our iterators are always constant
    const char_t *begin() const {
        return this->base_ + this->start_;
    }
    
    const char_t *end() const {
        return this->begin() + this->length();
    }


    // Constructor from std::string. Note this borrows the storage so we must not
    // outlive it.
    template <typename stdchar_t>
    explicit rstring_t(const std::basic_string<stdchar_t> &b, size_t start = 0,
                       size_t length = npos)
        : start_(start), length_(std::min(length, b.length() - start)), base_(b.c_str()) {}

    explicit rstring_t(const char_t *s, size_t len) : start_(0), length_(len), base_(s) {}
};

// SEPARATOR-NOTE
// See IEEE Std 1003.1-2008 Utility Argument Syntax
// http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap12.html
// Each option_t stores a separator, which describes how the option
// is separated from its option-argument (what we call value)
// Each may be space, =, or none
// Following the conventions, we allow the following rules:
//  - Short option-arguments may be space-separated or unseparated,
//    but not = separated
//  - Long (single or double) options may be space separated or = separated,
//    but not unseparated
//
//  There seems to be disagremeent in how single-long options take their
//  option-arguments. For example, `gcc -std=c89` requires an = separator,
//  while Go's flags parser is relaxed. For now we require strict separator
//  agreement for single-long options (e.g. -std c89 is disallowed).

// An option represents something like '--foo=bar'
// A single option may have multiple names, e.g. -f --foo
struct option_t {
    // The types of names
    // This is also used indexes into an array
    enum name_type_t {
        single_short,  // -f
        single_long,   // -foo
        double_long,   // --foo

        NAME_TYPE_COUNT
    };

    std::array<rstring_t, NAME_TYPE_COUNT> names;

    // value of the option, i.e. variable name. Empty for no value.
    rstring_t value;

    // Default value. Empty for none.
    rstring_t default_value;

    // special flags
    // these can only be set via annotated options today
    option_flags_t flags = 0;
    
public:

    option_t() {}

    option_t(name_type_t type, const rstring_t &name, const rstring_t &v, option_flags_t f)
        : value(v), flags(f) {
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
        return !this->names[type].empty();
    }

    // Returns the "best" (longest) name
    const rstring_t &best_name() const {
        size_t idx = NAME_TYPE_COUNT;
        while (idx--) {
            const rstring_t &name = this->names[idx];
            if (!name.empty()) {
                return name;
            }
        }
        return this->names[single_short];  // is empty
    }

    // Indicates where this option takes a value
    bool has_value() const {
        return ! value.empty();
    }
    
    // Returns whether any of our names matches
    bool has_name(const rstring_t &name) const {
        assert(! name.empty());
        const auto where = std::find(this->names.cbegin(), this->names.cend(), name);
        return where != this->names.end();
    }

    // Returns true if the options have the same name
    bool has_same_name(const option_t &opt2) const {
        bool result = false;
        for (size_t i = 0; i < NAME_TYPE_COUNT; i++) {
            const rstring_t &s1 = this->names[i], &s2 = opt2.names[i];
            if (s1.length() > 0 && s1 == s2) {
                result = true;
                break;
            }
        }
        return result;
    }
    
    // Helper function for dumping
    string_t describe() const {
        string_t result;
        rstring_t name = this->best_name();
        result.append(name.std_string());

        if (!value.empty()) {
            result.push_back(':');
            result.push_back(' ');
            result.append(value.std_string());
        }
        if (this->flags & value_is_optional) {
            result.push_back('?');
        }

        result.push_back(' ');

        if (this->flags & single_long_strict_eqsep) {
            result += STRCONSTANT(" (= sep)");
        }

        return result;
    }

    // Acquire "guts" from another option wherever we have blanks
    void merge_from(const option_t &rhs) {
        for (size_t i = 0; i < NAME_TYPE_COUNT; i++) {
            this->names[i].replace_if_empty(rhs.names[i]);
        }
        this->flags |= rhs.flags;
        this->value.replace_if_empty(rhs.value);
        this->default_value.replace_if_empty(rhs.default_value);
    }

    // Given a string and the inout range 'remaining', parse out an option and
    // return it.
    // Update the remaining range to reflect the number of characters used.
    static bool parse_from_string(rstring_t *remaining, option_t *result,
                                  std::vector<error_t> *errors = NULL);

    // Variant for when the remaining range is uninteresting.
    static bool parse_from_string(rstring_t str, option_t *result,
                                  std::vector<error_t> *errors = NULL) {
        return parse_from_string(&str, result, errors);
    }

    /* Given an argument (i.e. from argv), produce an option. */
    static option_t parse_from_argument(const string_t &str, option_t::name_type_t type);
};
typedef std::vector<option_t> option_list_t;

inline void append_error(std::vector<error_t> *errors, size_t location, int code, const char *text,
                         size_t arg_idx = -1) {
    if (errors != nullptr) {
        errors->push_back(error_t{location, arg_idx, code, text});
    }
}


// Internal flags
enum {
    // When matching, if we run out of positionals or options, instead of failing,
    // return a match containing a suggestion
    flag_generate_suggestions = 1U << 16,

    // When matching, if we consume all positionals and options, stop searching.
    flag_stop_after_consuming_everything = 1U << 17
};

// Error codes
enum {
    error_none,

    /* Errors that may occur in a docopt description */
    error_excessive_dashes,                      // Three or more dashes in an option: prog ---foo
    error_leading_ellipsis,                      // ... appearing without an associated expression
    error_excessive_equal_signs,                 // Two or more equal signs: --foo==bar
    error_bad_option_separator,                  // Bad separator between option and value:
                                                 // --foo<bar>
    error_invalid_option_name,                   // Bad option name: Options: foo
    error_invalid_variable_name,                 // Bad variable name: Options: --foo=
    error_missing_close_paren,                   // Missing ): prog (foo
    error_missing_close_bracket,                 // Missing ]:  prog [foo
    error_empty_bracket_paren,                   // Empty brackets or parens, like prog []
    error_missing_close_bracket_in_default,      // No close bracket. Options: --send
                                                 // <msg> Specifies
                                                 // message [default: none
    error_missing_close_variable,                // Missing >: <foo
    error_one_variable_multiple_commands,        // Two commands for same var. <msg> foo
                                                 // \n <msg> foo
    error_option_duplicated_in_options_section,  // Options: --foo, --foo
    error_trailing_vertical_bar,                 // Usage: prog foo | bar |
    error_unknown_leader,                        // Unknown leader on a line, e.g. leading ;

    // Errors that may occur in arguments (argv)
    // Lower values are more "likely" errors
    error_option_has_missing_argument =
        100,                           // Option expects an argument, but none was given in argv
    error_option_unexpected_argument,  // Option does not expect an argument, but
                                       // one was given in
                                       // argv
    error_ambiguous_prefix_match,      // Prefix matching was requested and the result
                                       // was ambiguous
    error_unknown_option,              // Option is not present in usage
    error_wrong_separator              // Wrong sort of separator
};

// A positional argument like 'checkout'
// This tracks its index in argv
struct positional_argument_t {
    size_t idx_in_argv;
    explicit positional_argument_t(size_t idx) : idx_in_argv(idx) {}
};
typedef std::vector<positional_argument_t> positional_argument_list_t;


// A resolved option references an option in argv
struct resolved_option_t {
    // The option from the usage referenced by this
    option_t option;
    
    // The index (in argv) of the name portion of the option
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

// Type that tracks how we classify arguments into positionals, options, and --
struct arg_classification_t {
    positional_argument_list_t positionals;
    resolved_option_list_t resolved_options;
    size_t double_dash_idx = -1;
};

typedef std::vector<size_t> index_list_t;
typedef std::map<rstring_t, argument_t> option_rmap_t;

// The result of matching
struct match_results_t {
    // Index list of unused arguments
    index_list_t unused_args;
    
    // List of options
    option_rmap_t option_map;
    
    // Set of suggestions.
    // This is only generated if flag_generate_suggestions is set
    std::set<rstring_t> suggestions;
    
};

struct usage_t;
match_results_t match_usages(const std::vector<usage_t> &usages,
                             parse_flags_t flags,
                             const option_list_t &shortcut_options,
                             const arg_classification_t &aclass,
                             const string_list_t &argv);


CLOSE_DOCOPT_IMPL

#endif
