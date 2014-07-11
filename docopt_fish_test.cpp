#include "docopt_fish.h"

#include <sstream>

using namespace docopt_fish;
using namespace std;

#define ERROR_EXPECTED "<ERROR>"

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

/* Splits up a const char * via a delimiter, returning a list of some string type */
template<typename string_t>
static vector<string_t> split(const char *str, char delimiter) {
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

template<typename string_t>
static void run_1_correctness_test(const char *usage, const char *joined_argv, const char *joined_expected_results) {
    /* Separate argv by spaces */
    const vector<string_t> argv = split<string_t>(joined_argv, ' ');
    
    /* Each line of joined_expected_results looks like "foo:bar". key:value. */
    const vector<string_t> expected = split<string_t>(joined_expected_results, '\n');
    
    /* Usage as a string */
    const string_t usage_str(usage, usage + strlen(usage));
    
    vector<size_t> unused_args;
    
    map<string_t, base_argument_t<string_t> > results = generic_docopt_parse(usage_str, argv, flag_generate_empty_args | flag_resolve_unambiguous_prefixes, &unused_args);
    
}

template<typename string_t>
static void test_correctness()
{
    struct args_t {
        const char * argv;
        const char * expected_results;
    };
    
    const struct testcase_t {
        const char *usage;
        args_t args[8];
    } testcases[] =
    {   /* Case 0 */
        {   "Usage: prog",
            {
                {   "",
                    ""
                },
                {   "--xxx",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 1 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -a  All.",
            {
                {   "",
                    "-a:False"
                },
                {   "-a",
                    "-a:True"
                },
                {   "-x",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 2 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: --all  All.",
            {
                {   "",
                    "--all:False"
                },
                {   "--all",
                    "--all:True"
                },
                {   "--xxx",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 3 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -v, --verbose  Verbose.",
            {
                {   "--verbose",
                    "--verbose:True"
                },
                {   "-v",
                    "--verbose:True"
                },
            },
        },
        /* Case 4 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p <PATH>",
            {
                {   "-p home/",
                    "-p:home/"
                },
                {   "-p",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 5 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p<PATH>",
            {
                {   "-phome/",
                    "-p:home/"
                },
                {   "-p",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 6 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: --path <path>",
            {
                {   "--path home/",
                    "--path:home/"
                },
                {   "--pa home/",
                    "--path:home/"
                },
                {   "--pa=home/",
                    "--path:home/"
                },
                {   "--path",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 7 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options: -p<PATH>, --path=<path>  Path to files.",
            {
                {   "-proot",
                    "--path:root"
                },
            },
        },
        /* Case 8 */
        {   "Usage: prog [options]\n"
            "\n"
            "Options:    -p <PATH>, --path <PATH>  Path to files.",
            {
                {   "-p root",
                    "--path:root"
                },
                {   "--path root",
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
                {   "",
                    "-p:./"
                },
                {   "-phome",
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
                {   "",
                    "--path:/root"
                },
                {   "--path=home",
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
                {   "-a -r -m Hello",
                    "-m:Hello\n"
                    "-a:True\n"
                    "-r:True"
                },
                {   "-arm yourass",
                    "-m:yourass\n"
                    "-a:True\n"
                    "-r:True"
                },
                {   "-a -r",
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
                {   "--version",
                    "--verbose:False\n"
                    "--version:True"
                },
                {   "--verbose",
                    "--verbose:True\n"
                    "--version:False"
                },
                {   "--ver",
                    ERROR_EXPECTED
                },
                {   "--verb",
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
                {   "-arm yourass",
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
                {   "-a -r -m Hello",
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
                {   "-a -b",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a",
                    ERROR_EXPECTED
                },
                {   "",
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
                {   "-a -b",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a",
                    ERROR_EXPECTED
                },
                {   "",
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
                {   "-a -b",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a",
                    ERROR_EXPECTED
                },
                {   "-b",
                    "-a:False\n"
                    "-b:True"
                },
                {   "",
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
                {   "-a -b",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-b -a",
                    "-a:True\n"
                    "-b:True"
                },
                {   "-a",
                    ERROR_EXPECTED
                },
                {   "-b",
                    ERROR_EXPECTED
                },
                {   "",
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
                {   "-a -b",
                    ERROR_EXPECTED
                },
                {   "",
                    ERROR_EXPECTED
                },
                {   "-a",
                    "-a:True\n"
                    "-b:False"
                },
                {   "-b",
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
                {   "-a -b",
                    ERROR_EXPECTED
                },
                {   "",
                    "-a:False\n"
                    "-b:False"
                },
                {   "-a",
                    "-a:True\n"
                    "-b:False"
                },
                {   "-b",
                    "-a:False\n"
                    "-b:True"
                },
            },
        },
        /* Case 21 */
        {   "usage: prog <arg>",
            {
                {   "10",
                    "<arg>:10"
                },
                {   "10 20",
                    ERROR_EXPECTED
                },
                {   "",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 22 */
        {   "usage: prog [<arg>]",
            {
                {   "10",
                    "<arg>:10"
                },
                {   "10 20",
                    ERROR_EXPECTED
                },
                {   "",
                    "<arg>:None"
                },
            },
        },
        /* Case 23 */
        {   "usage: prog <kind> <name> <type>",
            {
                {   "10 20 40",
                    "<type>:40\n"
                    "<kind>:10\n"
                    "<name>:20"
                },
                {   "10 20",
                    ERROR_EXPECTED
                },
                {   "",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 24 */
        {   "usage: prog <kind> [<name> <type>]",
            {
                {   "10 20 40",
                    "<type>:40\n"
                    "<kind>:10\n"
                    "<name>:20"
                },
                {   "",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 25 */
        {   "usage: prog [<kind> | <name> <type>]",
            {
                {   "10 20 40",
                    ERROR_EXPECTED
                },
                {   "20 40",
                    "<type>:40\n"
                    "<kind>:None\n"
                    "<name>:20"
                },
                {   "",
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
                {   "10 --all",
                    "--all:True\n"
                    "<kind>:10\n"
                    "<name>:None"
                },
                {   "10",
                    "--all:False\n"
                    "<kind>:None\n"
                    "<name>:10"
                },
                {   "",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 27 */
        {   "usage: prog [<name> <name>]",
            {
                {   "10 20",
                    "<name>:[u'10', u'20']"
                },
                {   "",
                    "<name>:None"
                },
            },
        },
        /* Case 28 */
        {   "usage: prog [(<name> <name>)]",
            {
                {   "10 20",
                    "<name>:[u'10', u'20']"
                },
                {   "10",
                    ERROR_EXPECTED
                },
                {   "",
                    "<name>:None"
                },
            },
        },
        /* Case 29 */
        {   "usage: prog <NAME>...",
            {
                {   "10 20",
                    "<NAME>:[u'10', u'20']"
                },
                {   "10",
                    "<NAME>:10"
                },
                {   "",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 30 */
        {   "usage: prog [<NAME>]...",
            {
                {   "10 20",
                    "<NAME>:[u'10', u'20']"
                },
                {   "10",
                    "<NAME>:10"
                },
                {   "",
                    "<NAME>:None"
                },
            },
        },
        /* Case 31 */
        {   "usage: prog [<NAME>...]",
            {
                {   "10 20",
                    "<NAME>:[u'10', u'20']"
                },
                {   "10",
                    "<NAME>:10"
                },
                {   "",
                    "<NAME>:None"
                },
            },
        },
        /* Case 32 */
        {   "usage: prog [<NAME> [<NAME> ...]]",
            {
                {   "10 20",
                    "<NAME>:[u'10', u'20']"
                },
                {   "10",
                    "<NAME>:10"
                },
                {   "",
                    "<NAME>:None"
                },
            },
        },
        /* Case 33 */
        {   "usage: prog (<NAME> | --foo <NAME>)\n"
            "\n"
            "options: --foo",
            {
                {   "10",
                    "--foo:False\n"
                    "<NAME>:10"
                },
                {   "--foo 10",
                    "--foo:True\n"
                    "<NAME>:10"
                },
                {   "--foo=10",
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
                {   "10",
                    "--bar:False\n"
                    "--foo:False\n"
                    "<NAME>:10"
                },
                {   "10 20",
                    "--bar:False\n"
                    "--foo:False\n"
                    "<NAME>:[u'10', u'20']"
                },
                {   "--foo --bar",
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
                {   "ship Guardian move 150 300 --speed=20",
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
                {   "--hello",
                    "--hello:True"
                },
            },
        },
        /* Case 37 */
        {   "usage: prog [--hello=<world>]",
            {
                {   "",
                    "--hello:False"
                },
                {   "--hello wrld",
                    "--hello:wrld"
                },
            },
        },
        /* Case 38 */
        {   "usage: prog [-o]",
            {
                {   "",
                    "-o:False"
                },
                {   "-o",
                    "-o:True"
                },
            },
        },
        /* Case 39 */
        {   "usage: prog [-o -p -r]",
            {
                {   "-op",
                    "-o:True\n"
                    "-p:True\n"
                    "-r:False"
                },
            },
        },
        /* Case 40 */
        {   "usage: prog --aabb | --aa",
            {
                {   "--aa",
                    "--aa:True\n"
                    "--aabb:False"
                },
                {   "--a",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 41 */
        {   "Usage: prog -v",
            {
                {   "-v",
                    "-v:True"
                },
            },
        },
        /* Case 42 */
        {   "Usage: prog [-v -v]",
            {
                {   "",
                    "-v:0"
                },
                {   "-v",
                    "-v:1"
                },
                {   "-vv",
                    "-v:2"
                },
            },
        },
        /* Case 43 */
        {   "Usage: prog -v ...",
            {
                {   "",
                    ERROR_EXPECTED
                },
                {   "-v",
                    "-v:1"
                },
                {   "-vv",
                    "-v:2"
                },
                {   "-vvvvvv",
                    "-v:6"
                },
            },
        },
        /* Case 44 */
        {   "Usage: prog [-v | -vv | -vvv]\n"
            "\n"
            "This one is probably most readable user-friednly variant.",
            {
                {   "",
                    "-v:False\n"
                    "-vvv:False\n"
                    "-vv:False"
                },
                {   "-v",
                    "-v:True\n"
                    "-vvv:False\n"
                    "-vv:False"
                },
                {   "-vv",
                    "-v:False\n"
                    "-vvv:False\n"
                    "-vv:True"
                },
                {   "-vvvv",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 45 */
        {   "usage: prog [--ver --ver]",
            {
                {   "--ver --ver",
                    "--ver:2"
                },
            },
        },
        /* Case 46 */
        {   "usage: prog [go]",
            {
                {   "go",
                    "go:True"
                },
            },
        },
        /* Case 47 */
        {   "usage: prog [go go]",
            {
                {   "",
                    "go:0"
                },
                {   "go go",
                    "go:2"
                },
                {   "go go go",
                    ERROR_EXPECTED
                },
            },
        },
        /* Case 48 */
        {   "usage: prog go...",
            {
                {   "go go go go go",
                    "go:5"
                },
            },
        },
        /* Case 49 */
        {   "usage: prog [options] [-a]\n"
            "\n"
            "options: -a\n"
            "         -b",
            {
                {   "-a",
                    "-a:True\n"
                    "-b:False"
                },
                {   "-aa",
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
                {   "arg",
                    "<A>:arg\n"
                    "-v:False\n"
                    "-q:False"
                },
                {   "-v arg",
                    "<A>:arg\n"
                    "-v:True\n"
                    "-q:False"
                },
                {   "-q arg",
                    "<A>:arg\n"
                    "-v:False\n"
                    "-q:True"
                },
            },
        },
        /* Case 51 */
        {   "usage: prog [-]",
            {
                {   "-",
                    "-:True"
                },
                {   "",
                    "-:False"
                },
            },
        },
        /* Case 52 */
        {   "usage: prog [<NAME> [<NAME> ...]]",
            {
                {   "a b",
                    "<NAME>:[u'a', u'b']"
                },
                {   "",
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
                {   "-a",
                    "-m:False\n"
                    "-a:True"
                },
            },
        },
        /* Case 54 */
        {   "usage: prog --hello",
            {
                {   "--hello",
                    "--hello:True"
                },
            },
        },
        /* Case 55 */
        {   "usage: prog [--hello=<world>]",
            {
                {   "",
                    "--hello:False"
                },
                {   "--hello wrld",
                    "--hello:wrld"
                },
            },
        },
        /* Case 56 */
        {   "usage: prog [-o]",
            {
                {   "",
                    "-o:False"
                },
                {   "-o",
                    "-o:True"
                },
            },
        },
        /* Case 57 */
        {   "usage: prog [-o -p -r]",
            {
                {   "-op",
                    "-o:True\n"
                    "-p:True\n"
                    "-r:False"
                },
            },
        },
        /* Case 58 */
        {   "usage: git [-v | --verbose]",
            {
                {   "-v",
                    "-v:True\n"
                    "--verbose:False"
                },
            },
        },
        /* Case 59 */
        {   "usage: git remote [-v | --verbose]",
            {
                {   "remote -v",
                    "-v:True\n"
                    "remote:True\n"
                    "--verbose:False"
                },
            },
        },
        /* Case 60 */
        {   "usage: prog",
            {
                {   "",
                    ""
                },
            },
        },
        /* Case 61 */
        {   "usage: prog\n"
            "           prog <a> <b>",
            {
                {   "1 2",
                    "<a>:1\n"
                    "<b>:2"
                },
                {   "",
                    "<a>:None\n"
                    "<b>:None"
                },
            },
        },
        /* Case 62 */
        {   "usage: prog <a> <b>\n"
            "           prog",
            {
                {   "",
                    "<a>:None\n"
                    "<b>:None"
                },
            },
        },
        /* Case 63 */
        {   "usage: prog [--file=<f>]",
            {
                {   "",
                    "--file:False"
                },
            },
        },
        /* Case 64 */
        {   "usage: prog [--file=<f>]\n"
            "\n"
            "options: --file <a>",
            {
                {   "",
                    "--file:False"
                },
            },
        },
        /* Case 65 */
        {   "Usage: prog [-a <host:port>]\n"
            "\n"
            "Options: -a, --address <host:port>  TCP address [default: localhost:6283].",
            {
                {   "",
                    "--address:localhost:6283"
                },
            },
        },
        /* Case 66 */
        {   "usage: prog --long=<arg> ...",
            {
                {   "--long one",
                    "--long:one"
                },
                {   "--long one --long two",
                    "--long:[u'one', u'two']"
                },
            },
        },
        /* Case 67 */
        {   "usage: prog (go <direction> --speed=<km/h>)...",
            {
                {   "go left --speed=5  go right --speed=9",
                    "go:2\n"
                    "<direction>:[u'left', u'right']\n"
                    "--speed:[u'5', u'9']"
                },
            },
        },
        /* Case 68 */
        {   "usage: prog [options] -a\n"
            "\n"
            "options: -a",
            {
                {   "-a",
                    "-a:True"
                },
            },
        },
        /* Case 69 */
        {   "usage: prog [-o <o>]...\n"
            "\n"
            "options: -o <o>  [default: x]",
            {
                {   "-o this -o that",
                    "-o:[u'this', u'that']"
                },
                {   "",
                    "-o:x"
                },
            },
        },
        /* Case 70 */
        {   "usage: prog [-o <o>]...\n"
            "\n"
            "options: -o <o>  [default: x y]",
            {
                {   "-o this",
                    "-o:this"
                },
                {   "",
                    "-o:x y"
                },
            },
        },
        /* Case 71 */
        {   "usage: prog -p<PATH>\n"
            "\n"
            "options: -p <PATH>",
            {
                {   "-pHOME",
                    "-p:HOME"
                },
            },
        },
        /* Case 72 */
        {   "Usage: foo (--xx=<x>|--yy=<y>)...",
            {
                {   "--xx=1 --yy=2",
                    "--yy:2\n"
                    "--xx:1"
                },
            },
        },
        /* Case 73 */
        {   "usage: prog [<input file>]",
            {
                {   "f.txt",
                    "<input file>:f.txt"
                },
            },
        },
        /* Case 74 */
        {   "usage: prog [--input=<file name>]...",
            {
                {   "--input a.txt --input=b.txt",
                    "--input:[u'a.txt', u'b.txt']"
                },
            },
        },
        /* Case 75 */
        {   "usage: prog good [options]\n"
            "           prog fail [options]\n"
            "\n"
            "options: --loglevel=<N>",
            {
                {   "fail --loglevel 5",
                    "fail:True\n"
                    "good:False\n"
                    "--loglevel:5"
                },
            },
        },
        /* Case 76 */
        {   "usage:prog --foo",
            {
                {   "--foo",
                    "--foo:True"
                },
            },
        },
        /* Case 77 */
        {   "PROGRAM USAGE: prog --foo",
            {
                {   "--foo",
                    "--foo:True"
                },
            },
        },
        /* Case 78 */
        {   "Usage: prog --foo\n"
            "           prog --bar\n"
            "NOT PART OF SECTION",
            {
                {   "--foo",
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
                {   "--foo",
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
                {   "--foo",
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
                {   "--baz --egg",
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
            run_1_correctness_test<string_t>(testcase->usage, args->argv, args->expected_results);
        }
    }
}

int main(int argc, const char** argv)
{
    test_correctness<string>();
    test_correctness<wstring>();
}
