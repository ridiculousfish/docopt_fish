DOCOPT_SRC_FILES=docopt_fish.cpp docopt_fish_parse_tree.cpp
TEST_SRC_FILES=${DOCOPT_SRC_FILES} docopt_fish_test.cpp
BENCHMARK_SRC_FILES=${DOCOPT_SRC_FILES} docopt_fish_benchmark.cpp
FUZZTARGET_SRC_FILES=${DOCOPT_SRC_FILES} docopt_fuzz_driver.cpp
HEADERS=docopt_fish.h docopt_fish_grammar.h docopt_fish_types.h
CXXFLAGS=-std=c++11 -g -W -Wall -Wunknown-pragmas

test: docopt_test docopt_wide_test
	./docopt_test ; ./docopt_wide_test

benchmark: docopt_benchmark
	./docopt_benchmark

# afl fuzzing target
fuzz: docopt_fuzztarget
	mkdir -p ./fuzz_output
	rm -rf ./fuzz_output/*
	afl-fuzz -i ./fuzz_input_samples -o ./fuzz_output ./$^

# Note we don't compile to object files first,
# because we have different preprocessor macros across
# the narrow and wide tests
docopt_test: ${TEST_SRC_FILES} ${HEADERS}
	${CXX} ${CXXFLAGS} ${TEST_SRC_FILES} -o $@

docopt_wide_test: ${TEST_SRC_FILES} ${HEADERS}
	${CXX} ${CXXFLAGS} -DDOCOPT_USE_WCHAR=1 ${TEST_SRC_FILES} -o $@

docopt_benchmark: ${BENCHMARK_SRC_FILES} ${HEADERS}
	${CXX} ${CXXFLAGS} -DDOCOPT_USE_WCHAR=1 -O3 ${BENCHMARK_SRC_FILES} -o $@

docopt_fuzztarget: ${FUZZTARGET_SRC_FILES} ${HEADERS}
	AFL_HARDEN=1 afl-clang++ ${CXXFLAGS} -Os ${FUZZTARGET_SRC_FILES} -o $@

python_test: run_testcase
	python ./run_tests.py

run_testcase: ${PY_TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${PY_TEST_SRC_FILES:.cpp=.o} -o $@

clean:
	rm -rf run_testcase docopt_test docopt_wide_test docopt_benchmark docopt_fuzztarget *.o *.dSYM fuzz_output

%.o: %.cpp
	${CXX} ${CXXFLAGS} $^ -c
