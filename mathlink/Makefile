MLINKDIR = /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-ARM64
SYS = MacOSX-ARM64
CADDSDIR = ${MLINKDIR}/CompilerAdditions

CXXFLAGS = -std=c++20 -stdlib=libstdc++ -stdlib=libc++ -stdlib=libc++ -Wall -g -ggdb -DPPERM_DEBUG -fsanitize=address -I${CADDSDIR}
LDFLAGS = -L${CADDSDIR} -lMLi4 -lm -fsanitize=address -framework Foundation

MPREP = ${CADDSDIR}/mprep

SRC = $(wildcard *.cc)

all: perm tests

perm: perm.cc.o mathlink.cc.o
	$(CXX) -o $@ $(LDFLAGS) $^

tests: tests.cc.o perm.cc.o
	$(CXX) -o $@ $(LDFLAGS) $^

%.cc.o: %.cc util.h perm.h
	$(CXX) -c -o $@ $(CXXFLAGS) $<

.PHONY: clean
clean:
	$(RM) *.o perm tests