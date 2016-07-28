#ifndef DOCOPT_FISH_H
#define DOCOPT_FISH_H

#include <string>
#include <vector>
#include <map>

namespace docopt_fish
{
    /* Our docopt classes are parameterized by a string, expected to be either std::wstring or std::string */
    enum {
        flags_default = 0U,
        
        /* If set, specifies that we should generate arguments even for unspecified values. These will have a count of 0. */
        flag_generate_empty_args = 1U << 0,
        
        /* If set, specifies that we should permit incomplete matching */
        flag_match_allow_incomplete = 1U << 1,
        
        /* If set, long options may be resolved using unambiguous prefixes. */
        flag_resolve_unambiguous_prefixes = 1U << 2,
        
        /* If set, short options that accept values must have the separator as specified in the usage, that is, -DNDEBUG and -D NDEBUG are not both allowed. */
        flag_short_options_strict_separators = 1U << 3
    };
    typedef unsigned int parse_flags_t;
    
    /* Status of an argument */
    enum argument_status_t {
        status_invalid, // the argument doesn't work
        status_valid, // the argument works
        status_valid_prefix // the argument is a prefix of something that may work
    };
    
    /* Represents an error. */
    struct error_t {
        /* Location of the token where the error occurred, in either the docopt doc or the argument */
        size_t location;
        
        /* If the error occurred in an argument (i.e. argv), the index of the argument; otherwise -1 */
        size_t argument_index;
        
        /* Internal code, for use in the tests */
        int code;
        
        /* Text of the error. This is an immortal string literal, but may someday need to be a std::string. */
        const char *text;
        
        error_t() : location(-1), argument_index(-1), code(0), text(NULL)
        {}
    };
    
    /* Represents an argument in the result */
    template<typename string_t>
    struct base_argument_t {
        /* The values specified in the argument. If this argument is a flag (like -d), this will be empty. If the argument has a single value, this will have a single value. If the argument has a default value, and no value was found in argv, the default will be contained in here (and count will be 0) */
        std::vector<string_t> values;
        
        /* How many times the argument appeared. This is typically 1 but may be greater than 1 for repeated arguments ("-v -v"), or 0 for missing arguments. */
        unsigned int count;
        
        /* Helper function to return a single value */
        const string_t &value() const {
            return values.at(0);
        }
        
        /* Constructor */
        base_argument_t() : count(0) {}
    };

    template<typename string_t>
    struct base_metadata_t {
        string_t command;
        string_t condition;
        string_t description;
        long tag; // arbitrary application use
        
        base_metadata_t() : tag(0) {}
    };
    
    /* A "direct" option for constructing arguments parsers programatically. */
    template<typename string_t>
    struct base_annotated_option_t
    {
        enum
        {
            single_short, // like -f
            single_long, // like -foo
            double_long, // like --foo
        } type;
        
        // The name of the option, including any dashes.
        // If empty, the type is ignored.
        string_t option;
        
        // Name of a variable representing the value, or empty
        // Separators are assumed to be flexible
        string_t value_name;
        
        // Metadata associated with the option
        // Note for historic reasons, this applies equally to both the option and variable
        base_metadata_t<string_t> metadata;
    };
    
    class docopt_impl;
    
    /* A processed docopt file is called an argument parser. */
    template<typename string_t>
    class argument_parser_t {
        /* Guts */
        docopt_impl *impl;
        
        public:
        
        typedef base_metadata_t<string_t> metadata_t;
        typedef base_annotated_option_t<string_t> annotated_option_t;
        typedef base_argument_t<string_t> argument_t;
        typedef std::map<string_t, argument_t> argument_map_t;
        typedef std::vector<error_t> error_list_t;
        
        /* Sets the docopt doc for this parser. Returns any parse errors by reference. Returns true if successful. */
        bool set_doc(const string_t &doc, error_list_t *out_errors);
        
        /* Sets the doc via a list of programmatically-specified options. The usage spec is assumed to be `prog [options]` */
        void set_options(const std::vector<annotated_option_t> &opts);
        
        /* Given a list of arguments, this returns a corresponding parallel array validating the arguments */
        std::vector<argument_status_t> validate_arguments(const std::vector<string_t> &argv, parse_flags_t flags) const;
        
        /* Given a list of arguments, returns an array of potential next values. A value may be either a literal flag -foo, or a variable; these may be distinguished by the <> surrounding the variable. */
        std::vector<string_t> suggest_next_argument(const std::vector<string_t> &argv, parse_flags_t flags) const;
        
        /** Given a name (either an option or a variable), returns any metadata for that name */
        metadata_t metadata_for_name(const string_t &name) const;
                
        /* Returns the list of command names (i.e. prog in `Usage: prog [options]`. Duplicate names are only returned once. */
        std::vector<string_t> get_command_names() const;

        /* Returns the list of variables like '<foo>'. Duplicate names are only returned once. */
        std::vector<string_t> get_variables() const;
        
        /* Given a list of arguments (argv), parse them, producing a map from option names to values */
        argument_map_t parse_arguments(const std::vector<string_t> &argv,
                        parse_flags_t flags,
                        error_list_t *out_errors = NULL,
                        std::vector<size_t> *out_unused_arguments = NULL) const;

        /* Constructor for when you either know the doc is error-free, or you aren't interested in the results, only the errors */
        argument_parser_t(const string_t &doc, error_list_t *out_errors);

        argument_parser_t();
        ~argument_parser_t();
        argument_parser_t(const argument_parser_t &rhs);
        argument_parser_t &operator=(const argument_parser_t &rhs);
    };
}

#endif
