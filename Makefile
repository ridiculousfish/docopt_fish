TEST_SRC_FILES=docopt_fish.cpp docopt_fish_test.cpp docopt_fish_parse_tree.cpp
HEADERS=docopt_fish.h docopt_fish_grammar.h docopt_fish_types.h
CXX=clang++
CXXFLAGS=-O0 -g -W -Wall

test: docopt_test
	./docopt_test

docopt_test: ${TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${TEST_SRC_FILES:.cpp=.o} -o $@

python_test: run_testcase
	python ./run_tests.py

run_testcase: ${PY_TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${PY_TEST_SRC_FILES:.cpp=.o} -o $@

clean:
	rm -f run_testcase docopt_test *.o

%.o: %.cpp
	${CXX} ${CXXFLAGS} $^ -c
