//
//  docopt_tests.cpp
//  docopt
//
//  Created by Jared Grubb on 2013-11-03.
//  Copyright (c) 2013 Jared Grubb. All rights reserved.
//

#include "docopt_fish.h"

#include <iostream>
#include <vector>

int main(int argc, const char** argv)
{	
	std::vector<std::string> args(argv+1, argv+argc);
    
    /* Read usage from stdin */
    std::string usage;
    char c;
    while (std::cin.get(c))
    {
        usage.push_back(c);
    }
	
	auto result = docopt::docopt(usage, args);
    
	// print it out in JSON form
	std::cout << "{ ";
	bool first = true;
	for(auto const& arg : result) {
		if (first) {
			first = false;
		} else {
			std::cout << "," << std::endl;
		}
		
		std::cout << '"' << arg.first << '"' << ": " << arg.second;
	}
	std::cout << " }" << std::endl;

	return 0;
}