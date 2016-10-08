TEST_SRC_FILES=docopt_fish.cpp docopt_fish_test.cpp docopt_fish_parse_tree.cpp
BENCHMARK_SRC_FILES=docopt_fish.cpp docopt_fish_benchmark.cpp docopt_fish_parse_tree.cpp
HEADERS=docopt_fish.h docopt_fish_grammar.h docopt_fish_types.h
CXXFLAGS=-std=c++11 -O3 -g -W -Wall -Wunknown-pragmas

test: docopt_test
	./docopt_test

benchmark: docopt_benchmark
	./docopt_benchmark

docopt_test: ${TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${TEST_SRC_FILES:.cpp=.o} -o $@

docopt_benchmark: ${BENCHMARK_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${BENCHMARK_SRC_FILES:.cpp=.o} -o $@

python_test: run_testcase
	python ./run_tests.py

run_testcase: ${PY_TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${PY_TEST_SRC_FILES:.cpp=.o} -o $@

clean:
	rm -f run_testcase docopt_test docopt_benchmark *.o

%.o: %.cpp
	${CXX} ${CXXFLAGS} $^ -c
