TEST_SRC_FILES=docopt_fish.cpp run_testcase.cpp
HEADERS=docopt_fish.h
CXX=clang++
CXXFLAGS=-O0 -g

run_testcase: ${TEST_SRC_FILES:.cpp=.o} ${HEADERS}
	${CXX} ${TEST_SRC_FILES:.cpp=.o} -o $@

test: run_testcase
	python ./run_tests.py

clean:
	rm -f run_testcase *.o

%.o: %.cpp
	${CXX} ${CXXFLAGS} $^ -c
