//
//  docopt_tests.cpp
//  docopt
//
//  Created by Jared Grubb on 2013-11-03.
//  Copyright (c) 2013 Jared Grubb. All rights reserved.
//

#include "docopt_fish.h"

using namespace docopt_fish;

#include <iostream>
#include <vector>

static std::string to_string(const argument_t &arg) {
    std::string result;
    if (arg.values.size() == 1) {
        result.push_back('"');
        result.append(arg.value());
        result.push_back('"');
    }
    else if (arg.values.size() > 1) {
        result.push_back('[');
        for (size_t i=0; i < arg.values.size(); i++) {
            if (i > 0) {
                result.append(", ");
            }
            result.push_back('"');
            result.append(arg.values.at(i));
            result.push_back('"');
        }
        result.push_back(']');
    } else if (arg.count > 1) {
        char buff[64];
        snprintf(buff, sizeof buff, "%u", arg.count);
        result.append(buff);
    } else if (arg.count == 1) {
        result.append("true");
    } else {
        result.append("false");
    }
    return result;
}

int main(int argc, const char** argv)
{	
	std::vector<std::string> args(argv+0, argv+argc);
    
    /* Read usage from stdin */
    std::string usage;
    char c;
    while (std::cin.get(c))
    {
        usage.push_back(c);
    }
	
    std::vector<size_t> unused_idxs;
	std::map<std::string, argument_t> result = docopt_parse(usage, args, flag_generate_empty_args | flag_resolve_unambiguous_prefixes, &unused_idxs);
    
    if (! unused_idxs.empty()) {
        std::cerr << "Unused arguments:\n";
        for (size_t i=0; i < unused_idxs.size(); i++) {
            std::cerr << argv[unused_idxs.at(i)] << '\n';
        }
        return EXIT_FAILURE;
    }
    
	// print it out in JSON form
	std::cout << "{ ";
	bool first = true;
    for (std::map<std::string, argument_t>::const_iterator iter = result.begin(); iter != result.end(); ++iter) {
		if (first) {
			first = false;
		} else {
			std::cout << "," << std::endl;
		}
		
		std::cout << '"' << iter->first << '"' << ": " << to_string(iter->second);
	}
	std::cout << " }" << std::endl;

	return 0;
}