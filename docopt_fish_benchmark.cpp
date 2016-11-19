#include <sys/time.h>
#include <cassert>
#include <string>
#include <vector>
#include "docopt_fish.h"

using namespace std;
using namespace docopt_fish;

static const char *const g_bind_usage =
    "Usage:\n"
    "    bind [-M <MODE> | --mode <MODE>] [-m <NEW_MODE> | --sets-mode "
    "<NEW_MODE>]\n"
    "         [-k | --key] <SEQUENCE> <COMMAND>...\n"
    "    bind [-M <MODE> | --mode <MODE>] [-k | --key] <SEQUENCE>\n"
    "    bind (-f | --function-names)\n"
    "    bind (-K | --key-names) [(-a | --all)]\n"
    "    bind (-e | --erase) [-M <MODE> | --mode <MODE>]\n"
    "         [-a | --all] [-k | --key] [<SEQUENCE>...]\n"
    "    bind (-h | --help)\n"
    "Options:\n"
    "    -k, --key                                Specify a key name, such as "
    "'left' or "
    "'backspace' instead of a character sequence.\n"
    "    -K, --key-names                          Display a list of available "
    "key names. "
    "Specifying -a or --all"
    "                                             includes keys that don't "
    "have a known mapping\n"
    "    -M <MODE>, --mode <MODE>                 Specify a bind mode that the "
    "bind is used in. "
    "Defaults to 'default'\n"
    "    -m <NEW_MODE>, --sets-mode <NEW_MODE>    Change the current mode to "
    "NEW_MODE after this "
    "binding is executed\n"
    "    -e, --erase                              Erase the binding with the "
    "given sequence and "
    "mode instead of defining a new one.\n"
    "                                             Multiple sequences can be "
    "specified with this "
    "flag. Specifying -a or --all with -M or --mode\n"
    "                                             erases all binds in the "
    "given mode regardless of "
    "sequence. Specifying -a or --all\n"
    "                                             without -M or --mode erases "
    "all binds in all "
    "modes regardless of sequence.\n"
    "    -a, --all                                See --erase and "
    "--key-names.\n";

static double timef() {
    int time_res;
    struct timeval tv;

    time_res = gettimeofday(&tv, 0);

    if (time_res) {
        return -1;
    }

    return (double)tv.tv_sec + 0.000001 * tv.tv_usec;
}

int main(int argc, char *argv[]) {
    size_t amt = 5000;
    double before, after;
    if (argc > 1) {
        amt = strtoul(argv[1], NULL, 0);
    }

    const docopt_fish::string_t bind_usage(g_bind_usage, g_bind_usage + strlen(g_bind_usage));
    before = timef();
    argument_parser_t parser;
    for (size_t i = 0; i < amt || false; i++) {
        parser.set_doc(bind_usage, NULL);
    }
    after = timef();
    fprintf(stderr, "construct msec per: %f\n", (after - before) * (1000.0) / amt);

    bool parsed = parser.set_doc(bind_usage.c_str(), NULL);
    assert(parsed);

    vector<wstring> doc_argv = {L"bind", L"abc", L"forward-word", L"--mode", L"derp"};

    before = timef();
    for (size_t i = 0; i < amt || false; i++) {
        parser.parse_arguments(doc_argv, flags_default, NULL, NULL);
    }
    after = timef();
    fprintf(stderr, "argv msec per: %f\n", (after - before) * (1000.0) / amt);
    return 0;
}
