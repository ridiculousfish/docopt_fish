#include "docopt_fish.h"

#include <sstream>
#include <assert.h>
#include <stdio.h>

using namespace docopt_fish;
using namespace std;

#define ERROR_EXPECTED "<ERROR>"

/**
 Print formatted error string
 */
static size_t err_count;

__attribute__ ((format (printf, 1, 2)))
static void err(const char *blah, ...)
{
    static bool has_term = !! getenv("TERM");
    va_list va;
    va_start(va, blah);
    err_count++;
    
    // show errors in red
    if (has_term) {
        fputs("\x1b[31m", stdout);
    }
    
    printf("Error: ");
    vprintf(blah, va);
    va_end(va);
    
    if (has_term) {
        // return to normal color
        fputs("\x1b[0m", stdout);
    }
    
    printf("\n");
}

/* Helpers to make a wide C string from a wide or narrow string */
static const wchar_t *widen(const wstring &t) {
    return t.c_str();
}

static const wchar_t *widen(const string &t) {
    static wstring result[16];
    static size_t idx = 0;
    idx = (idx + 1) % 16;
    result[idx].erase();
    result[idx].insert(result[idx].begin(), t.begin(), t.end());
    return result[idx].c_str();
}

template<typename string_t>
string_t to_string(const char *x) {
    string_t result;
    result.insert(result.begin(), x, x + strlen(x));
    return result;
}


#define do_test(e) do { if (! (e)) err("Test failed on line %lu: %s", __LINE__, #e); } while (0)
#define do_arg_test(e) do { if (! (e)) err("Test %lu.%lu failed on line %ld: %s", test_idx, arg_idx, (long)__LINE__, #e); } while (0)

map<string, base_argument_t<string> >
generic_docopt_parse(const string &doc,
                     const vector<string> &argv,
                     parse_flags_t flags,
                     vector<size_t> *out_unused_arguments)
{
    return docopt_parse(doc, argv, flags, out_unused_arguments);
}

map<wstring, base_argument_t<wstring> >
generic_docopt_parse(const wstring &doc,
                     const vector<wstring> &argv,
                     parse_flags_t flags,
                     vector<size_t> *out_unused_arguments)
{
    return docopt_wparse(doc, argv, flags, out_unused_arguments);
}

/* Splits up a string via a delimiter string, returning a list of some string type */
template<typename string_t>
static vector<string_t> split(const string_t &str, const char *delim) {
    vector<string_t> result;
    const string_t delim_string = to_string<string_t>(delim);
    size_t cursor = 0;
    while (cursor < str.size()) {
        size_t delim_pos = str.find(delim_string, cursor);
        if (delim_pos == string_t::npos) {
            delim_pos = str.size();
        }
        result.push_back(str.substr(cursor, delim_pos - cursor));
        cursor = delim_pos + delim_string.size();
    }
    return result;
}

/* Joins a vector of strings via a delimiter */
template<typename string_t>
string_t join(const vector<string_t> &vec, const char *delim) {
    string_t result;
    const string_t delim_string = to_string<string_t>(delim);
    for (size_t i=0; i < vec.size(); i++) {
        if (i > 0) {
            result.append(delim_string);
        }
        result.append(vec.at(i));
    }
    return result;
}

template<typename string_t>
static bool is_percent_digit(const string_t &val) {
    return val.size() == 2 && val.at(0) == '%' && val.at(1) >= '0' && val.at(1) <= '9';
}

template<typename string_t>
static vector<string_t> split_nonempty(const char *str, char delimiter) {
    vector<string_t> result;
    const char *cursor = str;
    for (;;) {
        const char *next = strchr(cursor, delimiter);
        if (next == NULL) {
            next = cursor + strlen(cursor);
        }
        
        /* Insert the token if it is not empty */
        if (next > cursor) {
            result.push_back(string_t(cursor, next));
        }
        
        // Skip over delimeters
        while (*next == delimiter) {
            next++;
        }
        // Break if we're done
        if (*next == '\0') {
            break;
        }
        cursor = next;
    }
    return result;
}

/* joined_expected_results is a newline-delimited "map" of the form key:value. Make a real map out of it. */
template<typename string_t>
static map<string_t, string_t> parse_expected_results(const char *joined_expected_results) {
    map<string_t, string_t> expected;
    vector<string_t> expected_lines = split_nonempty<string_t>(joined_expected_results, '\n');
    for (size_t i=0; i < expected_lines.size(); i++) {
        const string_t &str = expected_lines.at(i);
        size_t colon = str.find(':');
        assert(colon != string_t::npos);
        
        const string_t key(str, 0, colon);
        const string_t value(str, colon + 1);
        expected[key] = value;
    }
    return expected;
}


template<typename string_t>
static void run_1_suggestion_test(const char *usage, const char *joined_argv, const char *joined_expected_suggestions, size_t test_idx, size_t arg_idx) {
    using namespace docopt_fish;
    
    /* Separate argv by spaces */
    vector<string_t> argv = split_nonempty<string_t>(joined_argv, ' ');
    
    /* Prepend the program name for every argument */
    argv.insert(argv.begin(), to_string<string_t>("prog"));
    
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    std::vector<error_t<string_t> > errors;
    argument_parser_t<string_t> *parser = argument_parser_t<string_t>::create(usage_str, &errors);
    
    if (! errors.empty()) {
        err("Test %lu.%lu was expected to succeed, but instead errored:", test_idx, arg_idx);
        for (size_t i=0; i < errors.size(); i++) {
            fprintf(stderr, "\t%ls\n", widen(errors.at(i).text));
        }
    } else {
        /* Get the suggested arguments, then sort and join them */
        std::vector<string_t> suggestions = parser->suggest_next_argument(argv, flags_default);
        sort(suggestions.begin(), suggestions.end());
        const string_t sugg_string = join(suggestions, ", ");
        
        /* Split, sort, and join expected suggestions */
        std::vector<string_t> expected_vector = split(to_string<string_t>(joined_expected_suggestions), ", ");
        sort(expected_vector.begin(), expected_vector.end());
        const string_t expected_string = join(expected_vector, ", ");
        
        if (sugg_string != expected_string) {
            err("Test %lu.%lu: Wrong suggestions. Expected '%ls', got '%ls'", test_idx, arg_idx, widen(expected_string), widen(sugg_string));
        }
    }
}

template<typename string_t>
static void run_1_correctness_test(const char *usage, const char *joined_argv, const char *joined_expected_results, size_t test_idx, size_t arg_idx) {
    typedef map<string_t, base_argument_t<string_t> > arg_map_t;
    typedef map<string_t, string_t> string_map_t;
    
    /* Separate argv by spaces */
    vector<string_t> argv = split_nonempty<string_t>(joined_argv, ' ');
    
    /* Prepend the program name for every argument */
    argv.insert(argv.begin(), to_string<string_t>("prog"));
    
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    /* Perform the parsing */
    vector<size_t> unused_args;
    arg_map_t results = generic_docopt_parse(usage_str, argv, flag_generate_empty_args | flag_resolve_unambiguous_prefixes, &unused_args);
    
    bool expects_error = ! strcmp(joined_expected_results, ERROR_EXPECTED);
    bool did_error = ! unused_args.empty();
    
    if (expects_error && ! did_error) {
        err("Test %lu.%lu was expected to fail, but did not", test_idx, arg_idx);
    } else if (did_error && ! expects_error) {
        err("Test %lu.%lu was expected to succeed, but instead errored", test_idx, arg_idx);
    } else if (! did_error && ! expects_error) {
        /* joined_expected_results is a newline-delimited "map" of the form key:value */
        const string_map_t expected = parse_expected_results<string_t>(joined_expected_results);
        
        // Verify everything we expected appears in our map
        for (typename string_map_t::const_iterator iter = expected.begin(); iter != expected.end(); ++iter) {
            const string_t &key = iter->first;
            const string_t &val = iter->second;
            typename arg_map_t::const_iterator arg_iter = results.find(key);
            if (arg_iter == results.end()) {
                err("Test %lu.%lu: Expected to find %ls = %ls, but it was missing", test_idx, arg_idx, widen(key), widen(val));
            } else {
                const base_argument_t<string_t> &arg = arg_iter->second;
                /* The value here can be interpreted a few ways. If it is "True" or "False", it means we expect the argument to have no values, and to have a count of 1 or 0, respectively. If it is % followed by a one-digit number, it represents the count of the argument; values is expected to be empty. Otherwise, split the value about ', '; those are the values we expect. */
                if (val == to_string<string_t>("True")) {
                    /* A "true" means we expect the argument ot have no values, and to have appeared once */
                    do_arg_test(arg.count == 1);
                    do_arg_test(arg.values.empty());
                } else if (val == to_string<string_t>("False") || val == to_string<string_t>("None")) {
                    do_arg_test(arg.count == 0);
                    do_arg_test(arg.values.empty());
                } else if (is_percent_digit(val)) {
                    size_t expected_count = val.at(1) - '0';
                    assert(expected_count <= 9);
                    do_arg_test(arg.count == expected_count);
                    do_arg_test(arg.values.empty());
                } else {
                    const vector<string_t> values = split(val, ", ");
                    do_arg_test(arg.values == values);
                    /* Note that we don't test count here, since count will be 0 if it came from a default: clause, and nonzero if it did not */
                }
            }
        }
        
        // Verify nothing we didn't expect appears in our map
        for (typename arg_map_t::const_iterator iter = results.begin(); iter != results.end(); ++iter) {
            const string_t &key = iter->first;
            typename string_map_t::const_iterator result = expected.find(key);
            if (result == expected.end()) {
                err("Test %lu.%lu: Unexpected key %ls", test_idx, arg_idx, widen(key));
            }
        }
    }
}

template<typename string_t>
static void run_1_unused_argument_test(const char *usage, const char *joined_argv, const char *joined_expected_unused, size_t test_idx, size_t arg_idx) {
    typedef map<string_t, base_argument_t<string_t> > arg_map_t;
    typedef map<string_t, string_t> string_map_t;
    
    /* Separate argv by spaces */
    vector<string_t> argv = split_nonempty<string_t>(joined_argv, ' ');
    
    /* Prepend the program name for every argument */
    argv.insert(argv.begin(), to_string<string_t>("prog"));
    
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    /* Perform the parsing */
    vector<size_t> unused_arg_idxs;
    arg_map_t results = generic_docopt_parse(usage_str, argv, flag_generate_empty_args | flag_resolve_unambiguous_prefixes, &unused_arg_idxs);
    
    /* Construct unused argument string */
    vector<string_t> unused_args_vec;
    for (size_t i=0; i < unused_arg_idxs.size(); i++) {
        size_t idx = unused_arg_idxs.at(i);
        unused_args_vec.push_back(argv.at(idx));
    }
    sort(unused_args_vec.begin(), unused_args_vec.end());
    const string_t actual_unused = join(unused_args_vec, ", ");
    
    /* Compare unused arguments */
    std::vector<string_t> expected_unused_vec = split(to_string<string_t>(joined_expected_unused), ", ");
    sort(expected_unused_vec.begin(), expected_unused_vec.end());
    const string_t expected_unused = join(expected_unused_vec, ", ");
    
    if (expected_unused != actual_unused) {
        err("Test %lu.%lu: Wrong unused arguments. Expected '%ls', got '%ls'", test_idx, arg_idx, widen(expected_unused), widen(actual_unused));
    }

}

template<typename string_t>
static void run_1_condition_test(const char *usage, const char *variable, const char *expected_condition, size_t test_idx, size_t arg_idx) {
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    /* Perform the parsing */
    argument_parser_t<string_t> *parser = argument_parser_t<string_t>::create(usage_str, NULL);
    
    const string_t var_string(variable, variable + strlen(variable));
    const string_t condition_string = parser->conditions_for_variable(var_string);
    const string_t expected_condition_string(expected_condition, expected_condition + strlen(expected_condition));
    
    if (expected_condition_string != condition_string) {
        err("Test %lu.%lu: Wrong condition. Expected '%ls', got '%ls'", test_idx, arg_idx, widen(expected_condition_string), widen(condition_string));
    }
    
    delete parser;
}

template<typename string_t>
static void run_1_description_test(const char *usage, const char *option, const char *expected_description, size_t test_idx, size_t arg_idx) {
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    /* Perform the parsing */
    argument_parser_t<string_t> *parser = argument_parser_t<string_t>::create(usage_str, NULL);
    
    const string_t option_string(option, option + strlen(option));
    const string_t description_string = parser->description_for_option(option_string);
    const string_t expected_description_string(expected_description, expected_description + strlen(expected_description));
    
    if (expected_description_string != description_string) {
        err("Test %lu.%lu: Wrong description. Expected '%ls', got '%ls'", test_idx, arg_idx, widen(expected_description_string), widen(description_string));
    }
    
    delete parser;
}

struct args_t {
    const char * argv;
    const char * expected_results;
};

struct testcase_t {
    const char *usage;
    args_t args[8];
};

template<typename string_t>
static void test_correctness()
{
    const testcase_t testcases[] =
    {   /* Case 0 */
        {   "Usage: prog",
            {
                {   "", // argv
                    ""
                },
                {   "--xxx", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 1 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -a  All.",
            {
                {   "", // argv
                    "-a:False"
                },
                {   "-a", // argv
                    "-a:True"
                },
                {   "-x", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 2 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: --all  All.",
            {
                {   "", // argv
                    "--all:False"
                },
                {   "--all", // argv
                    "--all:True"
                },
                {   "--xxx", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 3 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -v, --verbose  Verbose.",
            {
                {   "--verbose", // argv
                    "--verbose:True"
                },
                {   "-v", // argv
                    "--verbose:True"
                },
            },
        },
        /* Case 4 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p <PATH>",
            {
                {   "-p home/", // argv
                    "-p:home/"
                },
                {   "-p", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 5 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p<PATH>",
            {
                {   "-phome/", // argv
                    "-p:home/"
                },
                {   "-p", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 6 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: --path <path>",
            {
                {   "--path home/", // argv
                    "--path:home/"
                },
                {   "--pa home/", // argv
                    "--path:home/"
                },
                {   "--pa=home/", // argv
                    "--path:home/"
                },
                {   "--path", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 7 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p<PATH>, --path=<path>  Path to files.",
            {
                {   "-proot", // argv
                    "--path:root"
                },
            },
        },
        /* Case 8 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options:    -p <PATH>, --path <PATH>  Path to files.",
            {
                {   "-p root", // argv
                    "--path:root"
                },
                {   "--path root", // argv
                    "--path:root"
                },
            },
        },
        /* Case 9 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options:\n"
            " -p<PATH>  Path to files [default: ./]",
            {
                {   "", // argv
                    "-p:./"
                },
                {   "-phome", // argv
                    "-p:home"
                },
            },
        },
        /* Case 10 */
        {   "UsAgE: prog [options]\n"
            "\n"
            "OpTiOnS: --path=<files>  Path to files\n"
            "                [dEfAuLt: /root]",
            {
                {   "", // argv
                    "--path:/root"
                },
                {   "--path=home", // argv
                    "--path:home"
                },
            },
        },
        /* Case 11 */
        {   "usage: prog [options]\n"
            "\n"
            "options:\n"
            "    -a        Add\n"
            "    -r        Remote\n"
            "    -m <msg>  Message",
            {
                {   "-a -r -m Hello", // argv
                    "-m:Hello\n"
                    "-a:True\n"
                    "-r:True"
                },
                {   "-arm yourass", // argv
                    "-m:yourass\n"
                    "-a:True\n"
                    "-r:True"
                },
                {   "-a -r", // argv
                    "-m:False\n"
                    "-a:True\n"
                    "-r:True"
                },
            },
        },
        /* Case 12 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: --version\n"
            "         --verbose",
            {
                {   "--version", // argv
                    "--verbose:False\n"
                    "--version:True"
                },
                {   "--verbose", // argv
                    "--verbose:True\n"
                    "--version:False"
                },
                {   "--ver", // argv
                    ERROR_EXPECTED
                },
                {   "--verb", // argv
                    "--verbose:True\n"
                    "--version:False"
                },
            },
        },
        /* Case 13 */
        {   "usage: prog [-a -r -m <msg>]\n"
            "\n"
            "options:\n"
            " -a        Add\n"
            " -r        Remote\n"
            " -m <msg>  Message",
            {
                {   "-arm yourass", // argv
                    "-m:yourass\n"
                    "-a:True\n"
                    "-r:True"
                },
            },
        },
        /* Case 14 */
        {   "usage: prog [-a -r -m <msg>]\n"
            "\n"
            "options: -a        Add\n"
            "         -r        Remote\n"
            "         -m <msg>  Message",
            {
                {   "-a -r -m Hello", // argv
                    "-m:Hello\n"
                    "-a:True\n"
                    "-r:True"
                },
            },
        },
        /* Case 15 */
        {   "usage: prog -a -b\n"
            "\n"
            "options:\n"
            " -a\n"
            " -b",
            {
                {   "-a -b", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 16 */
        {   "usage: prog (-a -b)\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a -b", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 17 */
        {   "usage: prog [-a] -b\n"
            "\n"
            "options: -a\n"
            " -b",
            {
                {   "-a -b", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a", // argv
                    ERROR_EXPECTED
                },
                {   "-b", // argv
                    "-a:False\n"
                    "-b:True"
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 18 */
        {   "usage: prog [(-a -b)]\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a -b", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a", // argv
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a", // argv
                    ERROR_EXPECTED
                },
                {   "-b", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    "-a:False\n"
                    "-b:False"
                },
            },
        },
        /* Case 19 */
        {   "usage: prog (-a|-b)\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a -b", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
                {   "-a", // argv
                    "-a:True\n"
                    "-b:False"
                },
                {   "-b", // argv
                    "-a:False\n"
                    "-b:True"
                },
            },
        },
        /* Case 20 */
        {   "usage: prog [ -a | -b ]\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a -b", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    "-a:False\n"
                    "-b:False"
                },
                {   "-a", // argv
                    "-a:True\n"
                    "-b:False"
                },
                {   "-b", // argv
                    "-a:False\n"
                    "-b:True"
                },
            },
        },
        /* Case 21 */
        {   "usage: prog <arg>",
            {
                {   "10", // argv
                    "<arg>:10"
                },
                {   "10 20", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 22 */
        {   "usage: prog [<arg>]",
            {
                {   "10", // argv
                    "<arg>:10"
                },
                {   "10 20", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    "<arg>:None"
                },
            },
        },
        /* Case 23 */
        {   "usage: prog <kind> <name> <type>",
            {
                {   "10 20 40", // argv
                    "<type>:40\n"
                    "<kind>:10\n"
                    "<name>:20"
                },
                {   "10 20", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 24 */
        {   "usage: prog <kind> [<name> <type>]",
            {
                {   "10 20 40", // argv
                    "<type>:40\n"
                    "<kind>:10\n"
                    "<name>:20"
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 25 */
        {   "usage: prog [<kind> | <name> <type>]",
            {
                {   "10 20 40", // argv
                    ERROR_EXPECTED
                },
                {   "20 40", // argv
                    "<type>:40\n"
                    "<kind>:None\n"
                    "<name>:20"
                },
                {   "", // argv
                    "<type>:None\n"
                    "<kind>:None\n"
                    "<name>:None"
                },
            },
        },
        /* Case 26 */
        {   "usage: prog (<kind> --all | <name>)\n"
            "\n"
            "options:\n"
            " --all",
            {
                {   "10 --all", // argv
                    "--all:True\n"
                    "<kind>:10\n"
                    "<name>:None"
                },
                {   "10", // argv
                    "--all:False\n"
                    "<kind>:None\n"
                    "<name>:10"
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 27 */
        {   "usage: prog [<name> <name>]",
            {
                {   "10 20", // argv
                    "<name>:10, 20"
                },
                {   "", // argv
                    "<name>:None"
                },
            },
        },
        /* Case 28 */
        {   "usage: prog [(<name> <name>)]",
            {
                {   "10 20", // argv
                    "<name>:10, 20"
                },
                {   "10", // argv
                    ERROR_EXPECTED
                },
                {   "", // argv
                    "<name>:None"
                },
            },
        },
        /* Case 29 */
        {   "usage: prog <NAME>...",
            {
                {   "10 20", // argv
                    "<NAME>:10, 20"
                },
                {   "10", // argv
                    "<NAME>:10"
                },
                {   "", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 30 */
        {   "usage: prog [<NAME>]...",
            {
                {   "10 20", // argv
                    "<NAME>:10, 20"
                },
                {   "10", // argv
                    "<NAME>:10"
                },
                {   "", // argv
                    "<NAME>:None"
                },
            },
        },
        /* Case 31 */
        {   "usage: prog [<NAME>...]",
            {
                {   "10 20", // argv
                    "<NAME>:10, 20"
                },
                {   "10", // argv
                    "<NAME>:10"
                },
                {   "", // argv
                    "<NAME>:None"
                },
            },
        },
        /* Case 32 */
        {   "usage: prog [<NAME> [<NAME> ...]]",
            {
                {   "10 20", // argv
                    "<NAME>:10, 20"
                },
                {   "10", // argv
                    "<NAME>:10"
                },
                {   "", // argv
                    "<NAME>:None"
                },
            },
        },
        /* Case 33 */
        {   "usage: prog (<NAME> | --foo <NAME>)\n"
            "\n"
            "options: --foo",
            {
                {   "10", // argv
                    "--foo:False\n"
                    "<NAME>:10"
                },
                {   "--foo 10", // argv
                    "--foo:True\n"
                    "<NAME>:10"
                },
                {   "--foo=10", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 34 */
        {   "usage: prog (<NAME> | --foo) [--bar | <NAME>]\n"
            "\n"
            "options: --foo\n"
            "options: --bar",
            {
                {   "10", // argv
                    "--bar:False\n"
                    "--foo:False\n"
                    "<NAME>:10"
                },
                {   "10 20", // argv
                    "--bar:False\n"
                    "--foo:False\n"
                    "<NAME>:10, 20"
                },
                {   "--foo --bar", // argv
                    "--bar:True\n"
                    "--foo:True\n"
                    "<NAME>:None"
                },
            },
        },
        /* Case 35 */
        {   "Naval Fate.\n"
            "\n"
            "Usage:\n"
            "  prog ship new <name>...\n"
            "  prog ship [<name>] move <x> <y> [--speed=<kn>]\n"
            "  prog ship shoot <x> <y>\n"
            "  prog mine (set|remove) <x> <y> [--moored|--drifting]\n"
            "  prog -h | --help\n"
            "  prog --version\n"
            "\n"
            "Options:\n"
            "  -h --help     Show this screen.\n"
            "  --version     Show version.\n"
            "  --speed=<kn>  Speed in knots [default: 10].\n"
            "  --moored      Mored (anchored) mine.\n"
            "  --drifting    Drifting mine.",
            {
                {   "ship Guardian move 150 300 --speed=20", // argv
                    "shoot:False\n"
                    "--moored:False\n"
                    "--drifting:False\n"
                    "move:True\n"
                    "--speed:20\n"
                    "mine:False\n"
                    "new:False\n"
                    "--version:False\n"
                    "set:False\n"
                    "remove:False\n"
                    "<name>:Guardian\n"
                    "ship:True\n"
                    "<x>:150\n"
                    "<y>:300\n"
                    "--help:False"
                },
            },
        },
        /* Case 36 */
        {   "usage: prog --hello",
            {
                {   "--hello", // argv
                    "--hello:True"
                },
            },
        },
        /* Case 37 */
        {   "usage: prog [--hello=<world>]",
            {
                {   "", // argv
                    "--hello:False"
                },
                {   "--hello wrld", // argv
                    "--hello:wrld"
                },
            },
        },
        /* Case 38 */
        {   "usage: prog [-o]",
            {
                {   "", // argv
                    "-o:False"
                },
                {   "-o", // argv
                    "-o:True"
                },
            },
        },
        /* Case 39 */
        {   "usage: prog [-o -p -r]",
            {
                {   "-op", // argv
                    "-o:True\n"
                    "-p:True\n"
                    "-r:False"
                },
            },
        },
        /* Case 40 */
        {   "usage: prog --aabb | --aa",
            {
                {   "--aa", // argv
                    "--aa:True\n"
                    "--aabb:False"
                },
                {   "--a", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 41 */
        {   "Usage: prog -v",
            {
                {   "-v", // argv
                    "-v:True"
                },
            },
        },
        /* Case 42 */
        {   "Usage: prog [-v -v]",
            {
                {   "", // argv
                    "-v:%0"
                },
                {   "-v", // argv
                    "-v:%1"
                },
                {   "-vv", // argv
                    "-v:%2"
                },
            },
        },
        /* Case 43 */
        {   "Usage: prog -v ...",
            {
                {   "", // argv
                    ERROR_EXPECTED
                },
                {   "-v", // argv
                    "-v:%1"
                },
                {   "-vv", // argv
                    "-v:%2"
                },
                {   "-vvvvvv", // argv
                    "-v:%6"
                },
            },
        },
        /* Case 44 */
        {   "Usage: prog [-v | -vv | -vvv]\n"
            "\n"
            "This one is probably most readable user-friednly variant.",
            {
                {   "", // argv
                    "-v:False\n"
                    "-vvv:False\n"
                    "-vv:False"
                },
                {   "-v", // argv
                    "-v:True\n"
                    "-vvv:False\n"
                    "-vv:False"
                },
                {   "-vv", // argv
                    "-v:False\n"
                    "-vvv:False\n"
                    "-vv:True"
                },
                {   "-vvvv", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 45 */
        {   "usage: prog [--ver --ver]",
            {
                {   "--ver --ver", // argv
                    "--ver:%2"
                },
            },
        },
        /* Case 46 */
        {   "usage: prog [go]",
            {
                {   "go", // argv
                    "go:True"
                },
            },
        },
        /* Case 47 */
        {   "usage: prog [go go]",
            {
                {   "", // argv
                    "go:%0"
                },
                {   "go go", // argv
                    "go:%2"
                },
                {   "go go go", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 48 */
        {   "usage: prog go...",
            {
                {   "go go go go go", // argv
                    "go:%5"
                },
            },
        },
        /* Case 49 */
        {   "usage: prog [options] [-a]\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a", // argv
                    "-a:True\n"
                    "-b:False"
                },
                {   "-aa", // argv
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 50 */
        {   "Usage: prog [options] <A>\n"
            "Options:\n"
            "    -q  Be quiet\n"
            "    -v  Be verbose.",
            {
                {   "arg", // argv
                    "<A>:arg\n"
                    "-v:False\n"
                    "-q:False"
                },
                {   "-v arg", // argv
                    "<A>:arg\n"
                    "-v:True\n"
                    "-q:False"
                },
                {   "-q arg", // argv
                    "<A>:arg\n"
                    "-v:False\n"
                    "-q:True"
                },
            },
        },
        /* Case 51 */
        {   "usage: prog [-]",
            {
                {   "-", // argv
                    "-:True"
                },
                {   "", // argv
                    "-:False"
                },
            },
        },
        /* Case 52 */
        {   "usage: prog [<NAME> [<NAME> ...]]",
            {
                {   "a b", // argv
                    "<NAME>:a, b"
                },
                {   "", // argv
                    "<NAME>:None"
                },
            },
        },
        /* Case 53 */
        {   "usage: prog [options]\n"
            "options:\n"
            " -a        Add\n"
            " -m <msg>  Message",
            {
                {   "-a", // argv
                    "-m:False\n"
                    "-a:True"
                },
            },
        },
        /* Case 54 */
        {   "usage: prog --hello",
            {
                {   "--hello", // argv
                    "--hello:True"
                },
            },
        },
        /* Case 55 */
        {   "usage: prog [--hello=<world>]",
            {
                {   "", // argv
                    "--hello:False"
                },
                {   "--hello wrld", // argv
                    "--hello:wrld"
                },
            },
        },
        /* Case 56 */
        {   "usage: prog [-o]",
            {
                {   "", // argv
                    "-o:False"
                },
                {   "-o", // argv
                    "-o:True"
                },
            },
        },
        /* Case 57 */
        {   "usage: prog [-o -p -r]",
            {
                {   "-op", // argv
                    "-o:True\n"
                    "-p:True\n"
                    "-r:False"
                },
            },
        },
        /* Case 58 */
        {   "usage: git [-v | --verbose]",
            {
                {   "-v", // argv
                    "-v:True\n"
                    "--verbose:False"
                },
            },
        },
        /* Case 59 */
        {   "usage: git remote [-v | --verbose]",
            {
                {   "remote -v", // argv
                    "-v:True\n"
                    "remote:True\n"
                    "--verbose:False"
                },
            },
        },
        /* Case 60 */
        {   "usage: prog",
            {
                {   "", // argv
                    ""
                },
            },
        },
        /* Case 61 */
        {   "usage: prog\n"
            "           prog <a> <b>",
            {
                {   "1 2", // argv
                    "<a>:1\n"
                    "<b>:2"
                },
                {   "", // argv
                    "<a>:None\n"
                    "<b>:None"
                },
            },
        },
        /* Case 62 */
        {   "usage: prog <a> <b>\n"
            "           prog",
            {
                {   "", // argv
                    "<a>:None\n"
                    "<b>:None"
                },
            },
        },
        /* Case 63 */
        {   "usage: prog [--file=<f>]",
            {
                {   "", // argv
                    "--file:False"
                },
            },
        },
        /* Case 64 */
        {   "usage: prog [--file=<f>]\n"
            "\n"
            "options: --file <a>",
            {
                {   "", // argv
                    "--file:False"
                },
            },
        },
        /* Case 65 */
        {   "Usage: prog [-a <host:port>]\n"
            "\n"
            "Options: -a, --address <host:port>  TCP address [default: localhost:6283].",
            {
                {   "", // argv
                    "--address:localhost:6283"
                },
            },
        },
        /* Case 66 */
        {   "usage: prog --long=<arg> ...",
            {
                {   "--long one", // argv
                    "--long:one"
                },
                {   "--long one --long two", // argv
                    "--long:one, two"
                },
            },
        },
        /* Case 67 */
        {   "usage: prog (go <direction> --speed=<km/h>)...",
            {
                {   "go left --speed=5  go right --speed=9", // argv
                    "go:%2\n"
                    "<direction>:left, right\n"
                    "--speed:5, 9"
                },
            },
        },
        /* Case 68 */
        {   "usage: prog [options] -a\n"
            "\n"
            "options: -a",
            {
                {   "-a", // argv
                    "-a:True"
                },
            },
        },
        /* Case 69 */
        {   "usage: prog [-o <o>]...\n"
            "\n"
            "options: -o <o>  [default: x]",
            {
                {   "-o this -o that", // argv
                    "-o:this, that"
                },
                {   "", // argv
                    "-o:x"
                },
            },
        },
        /* Case 70 */
        {   "usage: prog [-o <o>]...\n"
            "\n"
            "options: -o <o>  [default: x y]",
            {
                {   "-o this", // argv
                    "-o:this"
                },
                {   "", // argv
                    "-o:x y"
                },
            },
        },
        /* Case 71 */
        {   "usage: prog -p<PATH>\n"
            "\n"
            "options: -p <PATH>",
            {
                {   "-pHOME", // argv
                    "-p:HOME"
                },
            },
        },
        /* Case 72 */
        {   "Usage: foo (--xx=<x>|--yy=<y>)...",
            {
                {   "--xx=1 --yy=2", // argv
                    "--yy:2\n"
                    "--xx:1"
                },
            },
        },
        /* Case 73 */
        {   "usage: prog [<input file>]",
            {
                {   "f.txt", // argv
                    "<input file>:f.txt"
                },
            },
        },
        /* Case 74 */
        {   "usage: prog [--input=<file name>]...",
            {
                {   "--input a.txt --input=b.txt", // argv
                    "--input:a.txt, b.txt"
                },
            },
        },
        /* Case 75 */
        {   "usage: prog good [options]\n"
            "           prog fail [options]\n"
            "\n"
            "options: --loglevel=<N>",
            {
                {   "fail --loglevel 5", // argv
                    "fail:True\n"
                    "good:False\n"
                    "--loglevel:5"
                },
            },
        },
        /* Case 76 */
        {   "usage:prog --foo",
            {
                {   "--foo", // argv
                    "--foo:True"
                },
            },
        },
        /* Case 77 */
        {   "PROGRAM USAGE: prog --foo",
            {
                {   "--foo", // argv
                    "--foo:True"
                },
            },
        },
        /* Case 78 */
        {   "Usage: prog --foo\n"
            "           prog --bar\n"
            "NOT PART OF SECTION",
            {
                {   "--foo", // argv
                    "--bar:False\n"
                    "--foo:True"
                },
            },
        },
        /* Case 79 */
        {   "Usage:\n"
            " prog --foo\n"
            " prog --bar\n"
            "\n"
            "NOT PART OF SECTION",
            {
                {   "--foo", // argv
                    "--bar:False\n"
                    "--foo:True"
                },
            },
        },
        /* Case 80 */
        {   "Usage:\n"
            " prog --foo\n"
            " prog --bar\n"
            "NOT PART OF SECTION",
            {
                {   "--foo", // argv
                    "--bar:False\n"
                    "--foo:True"
                },
            },
        },
        /* Case 81 */
        {   "Usage: prog [options]\n"
            "\n"
            "global options: --foo\n"
            "local options: --baz\n"
            "               --bar\n"
            "other options:\n"
            " --egg\n"
            " --spam\n"
            "-not-an-option-",
            {
                {   "--baz --egg", // argv
                    "--baz:True\n"
                    "--bar:False\n"
                    "--spam:False\n"
                    "--egg:True\n"
                    "--foo:False"
                },
            },
        },
        {NULL, {}}
    }
    ;
    
    for (size_t testcase_idx=0; testcases[testcase_idx].usage != NULL; testcase_idx++) {
        const testcase_t *testcase = &testcases[testcase_idx];
        for (size_t arg_idx = 0; testcase->args[arg_idx].argv != NULL; arg_idx++) {
            const args_t *args = &testcase->args[arg_idx];
            run_1_correctness_test<string_t>(testcase->usage, args->argv, args->expected_results, testcase_idx, arg_idx);
        }
    }
}

template<typename string_t>
static void test_suggestions()
{
    const testcase_t testcases[] =
    {   /* Case 0 */
        {   "Usage: prog --status\n"
            "       prog jump [--height <in>]",
            {
                {   "", // argv
                    "jump, --status, --height"
                },
                {   "jump", // argv
                    "--height"
                },
                {   "jump --height", // argv
                    "<in>"
                },
            },
        },
        /* Case 1 */
        {   "Usage: prog --status\n"
            "       prog jump [--height <in>]",
            {
                {   "", // argv
                    "jump, --status, --height"
                },
                {   "jump", // argv
                    "--height"
                },
                {   "jump --height", // argv
                    "<in>"
                },
            },
        },
        /* Case 2 */
        {   "Usage: prog [fast] [options]\n"
            "Options: -f, --foo",
            {
                {   "", // argv
                    "fast, -f, --foo"
                },
                {   "fast", // argv
                    "-f, --foo"
                },
                {   "fast --foo", // argv
                    ""
                },
                {   "fast -f", // argv
                    ""
                },
            },
        },
        /* Case 3 */
        {   "Usage: cp [options] <src>... <dst>\n"
            "Options: --fast\n"
            "         --slow\n",
            {
                {   "", // argv
                    "<src>, <dst>, --fast, --slow"
                },
                {   "some_src", // argv
                    "<src>, <dst>, --fast, --slow"
                },
                {   "some_src --fast", // argv
                    "<src>, <dst>, --slow"
                },
                {   "some_src --slow", // argv
                    "<src>, <dst>, --fast"
                }
            },
        },
        /* Case 4 */
        {   "Usage: cp [--message <name>]\n",
            {
                {   "", // argv
                    "--message"
                },
                {   "--message", // argv
                    "<name>"
                },
            },
        },
        {NULL, {}}
    };
    for (size_t testcase_idx=0; testcases[testcase_idx].usage != NULL; testcase_idx++) {
        const testcase_t *testcase = &testcases[testcase_idx];
        for (size_t arg_idx = 0; testcase->args[arg_idx].argv != NULL; arg_idx++) {
            const args_t *args = &testcase->args[arg_idx];
            run_1_suggestion_test<string_t>(testcase->usage, args->argv, args->expected_results, testcase_idx, arg_idx);
        }
    }
}

template<typename string_t>
static void test_unused_args()
{
    const testcase_t testcases[] =
    {   /* Case 0 */
        {   "Usage: prog [--status]\n"
            "       prog jump [--height <in>]",
            {
                {   "", // argv
                    ""
                },
                {   "jump", // argv
                    ""
                },
                {   "--foo", // argv
                    "--foo"
                },
                {   "jump foo", // argv
                    "foo"
                },
            },
        },
        /* Case 1 */
        {   "Usage: prog [options] <pid>\n"
            "Options: --embiggen, -e  Embiggen the smallest man",
            {
                {   "12", // argv
                    ""
                },
                {   "12 -e", // argv
                    ""
                },
                {   "12 -e -e", // argv
                    "-e"
                },
                {   "12 --embiggen -e", // argv
                    "-e"
                },
            },
        },
        {NULL, {}}
    };
    for (size_t testcase_idx=0; testcases[testcase_idx].usage != NULL; testcase_idx++) {
        const testcase_t *testcase = &testcases[testcase_idx];
        for (size_t arg_idx = 0; testcase->args[arg_idx].argv != NULL; arg_idx++) {
            const args_t *args = &testcase->args[arg_idx];
            run_1_unused_argument_test<string_t>(testcase->usage, args->argv, args->expected_results, testcase_idx, arg_idx);
        }
    }
}

template<typename string_t>
static void test_descriptions()
{
    const testcase_t testcases[] =
    {
        /* Case 0 */
        {   "Usage: prog [options] <pid>\n"
            "Options: --embiggen, -e  Embiggen the smallest man",
            {
                {   "--embiggen",
                    "Embiggen the smallest man"
                },
                {   "-e",
                    "Embiggen the smallest man"
                },
                {   "-f",
                    ""
                },
                {   "--debiggen",
                    ""
                },
            },
        },
        {NULL, {}}
    };
    for (size_t testcase_idx=0; testcases[testcase_idx].usage != NULL; testcase_idx++) {
        const testcase_t *testcase = &testcases[testcase_idx];
        for (size_t arg_idx = 0; testcase->args[arg_idx].argv != NULL; arg_idx++) {
            const args_t *args = &testcase->args[arg_idx];
            run_1_description_test<string_t>(testcase->usage, args->argv, args->expected_results, testcase_idx, arg_idx);
        }
    }
}


template<typename string_t>
static void test_conditions()
{
    const testcase_t testcases[] =
    {   /* Case 0 */
        {   "Usage: prog <pid>\n"
            "Conditions: <pid>  get_a_pid",
            {
                {   "<pid>", // variable
                    "get_a_pid"
                }
            }
        },
    };
    for (size_t testcase_idx=0; testcases[testcase_idx].usage != NULL; testcase_idx++) {
        const testcase_t *testcase = &testcases[testcase_idx];
        for (size_t arg_idx = 0; testcase->args[arg_idx].argv != NULL; arg_idx++) {
            const args_t *args = &testcase->args[arg_idx];
            run_1_condition_test<string_t>(testcase->usage, args->argv, args->expected_results, testcase_idx, arg_idx);
        }
    }
}


template<typename string_t>
void do_all_tests() {
    test_correctness<string_t>();
    test_unused_args<string_t>();
    test_suggestions<string_t>();
    test_descriptions<string_t>();
    test_conditions<string_t>();
}

int main(int argc, const char** argv)
{
    do_all_tests<string>();
    do_all_tests<wstring>();
    printf("Encountered %lu errors in docopt tests\n", err_count);
    return err_count ? 1 : 0;
}
