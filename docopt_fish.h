#ifndef DOCOPT_FISH_H
#define DOCOPT_FISH_H

#include <map>
#include <string>
#include <vector>

namespace docopt_fish {
#ifdef DOCOPT_USE_WCHAR
typedef std::wstring string_t;
#else
typedef std::string string_t;
#endif
typedef std::vector<string_t> string_list_t;

enum {
    flags_default = 0U,

    // If set, specifies that we should permit incomplete matching
    flag_match_allow_incomplete = 1U << 0,

    // If set, short options that accept values must have the separator as specified in the usage,
    // that is, -DNDEBUG and -D NDEBUG are not both allowed.
    flag_short_options_strict_separators = 1U << 1
};
typedef unsigned int parse_flags_t;

// Status of an argument
enum argument_status_t {
    status_invalid,      // the argument doesn't work
    status_valid         // the argument works
};

// Represents an error in either the usage spec, or during argument parsing
struct error_t {
    // Location of the token where the error occurred, in either the docopt doc or the argument
    size_t location = size_t(-1);

    // If the error occurred in an argument (i.e. argv), the index of the argument; otherwise -1
    size_t argument_index = size_t(-1);

    // Internal code, for use in the tests
    int code = 0;

    // Text of the error. This is an immortal string literal, but may someday need to be a
    // std::string.
    const char *text = nullptr;
};

// Represents an argument in the result
struct argument_t {
    // The values specified in the argument. If this argument is a flag (like -d), this will be
    // empty. If the argument has a single value, this will have a single value. If the argument has
    // a default value, and no value was found in argv, the default will be contained in here (and
    // count will be 0)
    string_list_t values;

    // How many times the argument appeared. This is typically 1 but may be greater than 1 for
    // repeated arguments ("-v -v"), or 0 for missing arguments.
    unsigned int count = 0;

    // Helper function to return a single value
    const string_t &value() const {
        return values.at(0);
    }
};

template <typename STR>
struct base_metadata_t {
    STR command;
    STR condition;
    STR description;
    long tag;  // arbitrary application use

    base_metadata_t() : tag(0) {}
};

typedef base_metadata_t<string_t> metadata_t;
    
// Flags set for annotated options
enum {
    // Indicates that any value is optional
    // It is rare for values to be optional. An example is
    // the --backup option to cp, which may be specified alone
    // or may take an option.
    // Options with optional values may only use the = separator.
    value_is_optional = 1 << 0
};
typedef uint32_t annotated_option_flags_t;

// A "direct" option for constructing arguments parsers programatically.
struct annotated_option_t {
    enum type_t {
        value_only,    // no option, like ls <file>
        single_short,  // like -f
        single_long,   // like -foo
        double_long,   // like --foo
    } type;

    // The name of the option, including any dashes.
    string_t option;

    // Name of a variable representing the value, or empty
    // Separators are assumed to be flexible
    string_t value_name;

    // Flags controlling how the option is interpreted
    // These are mostly fish-specific
    annotated_option_flags_t flags;

    // Metadata associated with the option
    // Note for historic reasons, this applies equally to both the option and
    // variable
    metadata_t metadata;
};

// Suggestions are returned from suggest_next_argument and suggest_option_completion
struct suggestion_t {
    // The token to be inserted
    string_t token;
    
    // Metadata about the token
    metadata_t md;
    
    // Helpers for sorting
    bool operator==(const suggestion_t &rhs) const {
        return token == rhs.token;
    }
    
    bool operator<(const suggestion_t &rhs) const {
        return token < rhs.token;
    }

};
typedef std::vector<suggestion_t> suggestion_list_t;

class docopt_impl;

// A processed docopt file is called an argument parser.
class argument_parser_t {
    // Guts
    docopt_impl *impl;

    // No copying, move-semantics only
    argument_parser_t(const argument_parser_t &) = delete;
    argument_parser_t &operator=(const argument_parser_t &) = delete;

   public:
    typedef std::map<string_t, argument_t> argument_map_t;
    typedef std::vector<error_t> error_list_t;

    // Sets the docopt doc for this parser. Returns any parse errors by reference. Returns true if
    // successful.
    bool set_doc(string_t doc, error_list_t *out_errors);

    // Sets the doc via a list of programmatically-specified options. The usage spec is assumed to
    // be `prog [options]`
    void set_options(std::vector<annotated_option_t> opts);

    // Given a list of arguments, this returns a corresponding parallel array validating the
    // arguments
    std::vector<argument_status_t> validate_arguments(const string_list_t &argv,
                                                      parse_flags_t flags) const;

    // Given a list of arguments, returns an array of potential next values. A value may be either a
    // literal flag -foo, or a variable; these may be distinguished by the <> surrounding the
    // variable.
    suggestion_list_t suggest_next_argument(const string_list_t &argv, parse_flags_t flags) const;

    // Given a partial argument that may be an option, suggest completions for it
    // This handles unseparated or =-separated options
    // Each returned value may be an option, a literal separator (like =), or a variable like <file>
    suggestion_list_t suggest_option_completion(const string_t &arg, parse_flags_t flags) const;

    // Given a name (either an option or a variable), returns any metadata for that name
    metadata_t metadata_for_name(const string_t &name) const;

    // Returns the list of command names (i.e. prog in `Usage: prog [options]`. Duplicate names are
    // only returned once.
    string_list_t get_command_names() const;

    // Given a list of arguments (argv), parse them, producing a map from option names to values
    argument_map_t parse_arguments(const string_list_t &argv, parse_flags_t flags,
                                   error_list_t *out_errors = NULL,
                                   std::vector<size_t> *out_unused_arguments = NULL) const;

    // Constructor for when you either know the doc is error-free, or you aren't interested in the
    // results, only the errors
    argument_parser_t(const string_t &doc, error_list_t *out_errors);

    argument_parser_t();
    ~argument_parser_t();
    argument_parser_t(argument_parser_t &&rhs);
    argument_parser_t &operator=(argument_parser_t &&rhs);
};
}

#endif
