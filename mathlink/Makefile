MLINKDIR = /Applications/Mathematica.app/Contents/SystemFiles/Links/WSTP/DeveloperKit/MacOSX-ARM64
SYS = MacOSX-ARM64
CADDSDIR = ${MLINKDIR}/CompilerAdditions

CXXFLAGS = -std=c++20 -stdlib=libstdc++ -stdlib=libc++ -stdlib=libc++ -Wall -g -ggdb -DPPERM_DEBUG -fsanitize=address -I${CADDSDIR}
LDFLAGS = -L${CADDSDIR} -lWSTPi4 -lm -fsanitize=address -framework Foundation

MPREP = ${CADDSDIR}/wsprep

SRC = $(wildcard *.cc)
HEADERS = $(wildcard *.h)

all: pperm-ml tests

pperm-ml: perm.cc.o mathlink.cc.o
	$(CXX) -o $@ $(LDFLAGS) $^

tests: tests.cc.o perm.cc.o
	$(CXX) -o $@ $(LDFLAGS) $^

%.cc.o: %.cc $(HEADERS)
	$(CXX) -c -o $@ $(CXXFLAGS) $<

mathlink.tm.cc: mathlink.tm
	${MPREP} $? -o $@

.PHONY: clean
clean:
	$(RM) *.o *.tm.cc perm tests
