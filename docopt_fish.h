#include <string>
#include <vector>
#include <map>

namespace docopt_fish
{
    /* Our docopt classes are parameterized by a string, expected to be either std::wstring or std::string */
    
    enum {
        flags_default = 0,
        
        /* If set, specifies that we should generate arguments even for unspecified values. These will have a count of 0. */
        flag_generate_empty_args = 1 << 0,
        
        /* If set, long options may be resolved using unambiguous prefixes. */
        flag_resolve_unambiguous_prefixes = 1 << 1,
    };
    typedef unsigned int parse_flags_t;
    
    /* Represents an argument in the result */
    template<typename string_t>
    struct base_argument_t {
        /* How many times the argument appeared. This is typically 1 but may be greater than 1 for repeated arguments ("-v -v"), or 0 for missing arguments. */
        unsigned int count;
        
        /* The values specified in the argument. If this argument is a flag (like -d), this will be empty. If the argument has a single value, this will have a single value. */
        std::vector<string_t> values;
        
        /* Helper function to return a single value */
        const string_t &value() const {
            return values.at(0);
        }
        
        /* Constructor */
        base_argument_t() : count(0)
        {}
    };
    
    /* Concrete types */
    typedef base_argument_t<std::string> argument_t;
    typedef base_argument_t<std::wstring> wargument_t;
    
    /* Result of a docopt operation is a map from keys to values */
    std::map<std::string, argument_t> docopt_parse(const std::string &doc, const std::vector<std::string> &argv, parse_flags_t flags, std::vector<size_t> *out_unused_arguments = NULL);

    /* Wide variant */
    std::map<std::wstring, wargument_t> docopt_wparse(const std::wstring &doc, const std::vector<std::wstring> &argv, parse_flags_t flags, std::vector<size_t> *out_unused_arguments = NULL);
    
};