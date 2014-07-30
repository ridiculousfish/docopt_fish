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
    };
    typedef unsigned int parse_flags_t;
    
    /* Status of an argument */
    enum argument_status_t {
        status_invalid, // the argument doesn't work
        status_valid, // the argument works fine
        status_valid_prefix // the argument is a prefix of something that may work
    };
    
    /* Represents an error. */
    template<typename string_t>
    struct error_t {
        /* Location of the token where the error occurred */
        size_t location;
        
        /* Text of the error */
        string_t text;
    };
    
    /* A processed docopt file is called an argument parser. */
    template<typename string_t> class docopt_impl;
    
    template<typename string_t>
    class argument_parser_t {
        /* Guts */
        string_t src;
        docopt_impl<string_t> *impl;
        
        public:
        static argument_parser_t *create(const string_t &doc, std::vector<error_t<string_t> > *out_errors);
        
        /* Given a list of arguments, this returns a corresponding parallel array validating the arguments */
        std::vector<argument_status_t> validate_arguments(const std::vector<string_t> &argv, parse_flags_t flags);
        
        /* Given a list of arguments, returns an array of potential next values. A value may be either a literal flag -foo, or a variable; these may be distinguished by the <> surrounding the variable. */
        std::vector<string_t> suggest_next_argument(const std::vector<string_t> &argv, parse_flags_t flags);
        
        /* Given a variable name, returns the conditions for that variable, or the empty string if none. */
        string_t conditions_for_variable(const string_t &var) const;
        
        argument_parser_t();
        ~argument_parser_t();
    };
    
    
    /* Represents an argument in the result */
    template<typename string_t>
    struct base_argument_t {
        /* How many times the argument appeared. This is typically 1 but may be greater than 1 for repeated arguments ("-v -v"), or 0 for missing arguments. */
        unsigned int count;
        
        /* The values specified in the argument. If this argument is a flag (like -d), this will be empty. If the argument has a single value, this will have a single value. If the argument has a default value, and no value was found in argv, the default will be contained in here (and count will be 0) */
        std::vector<string_t> values;
        
        /* Helper function to return a single value */
        const string_t &value() const {
            return values.at(0);
        }
        
        /* Constructor */
        base_argument_t() : count(0) {}
    };
    
    /* Concrete types */
    typedef base_argument_t<std::string> argument_t;
    typedef base_argument_t<std::wstring> wargument_t;
    
    /* Result of a docopt operation is a map from keys to values */
    std::map<std::string, argument_t> docopt_parse(const std::string &doc, const std::vector<std::string> &argv, parse_flags_t flags, std::vector<size_t> *out_unused_arguments = NULL);

    /* Wide variant */
    std::map<std::wstring, wargument_t> docopt_wparse(const std::wstring &doc, const std::vector<std::wstring> &argv, parse_flags_t flags, std::vector<size_t> *out_unused_arguments = NULL);
    
};

#endif
