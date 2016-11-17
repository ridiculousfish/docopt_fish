#include <string>
#include <vector>
#include <cassert>
#include <sys/time.h>
#include "docopt_fish.h"

// reads input from stdin, parses it as docopt syntax

using namespace std;
using namespace docopt_fish;

int main()
{
    std::string text;
    char buff[1024];
    while (fgets(buff, sizeof buff, stdin)) {
        text.append(buff);
    }
    
    argument_parser_t::error_list_t errors;
    argument_parser_t parser(std::move(text), &errors);
    
    if (! errors.empty()) {
        for (const auto &err : errors) {
            printf("error (loc: %lu, idx: %lu): %s\n", err.location, err.argument_index, err.text);
        }
    } else {
        printf("No errors\n");
        std::vector<std::string> args = {"foo", "--bar=baz", "--ack"};
        docopt_fish::parse_flags_t flags = 0;
        parser.validate_arguments(args, flags);
        parser.suggest_next_argument(args, flags);
        parser.get_command_names();
        
        argument_parser_t::error_list_t argv_errors;
        std::vector<size_t> unused_args;
        auto argmap = parser.parse_arguments(args, flags, &argv_errors, &unused_args);
        printf("Got %lu arguments\n", argmap.size());
    }
    
    return 0;
}
