ifeq ($(OS),Windows_NT)

else
	UNAME_S := $(shell uname -s)
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_S),Linux)
		ifeq ($(UNAME_P),x86_64)
			DEFAULT_SYS := Linux-x86-64
		endif
		ifneq ($(filter arm%,$(UNAME_P)),)

		endif
    endif
    ifeq ($(UNAME_S),Darwin)
		DEFAULT_MLINKDIR := /Applications/Mathematica.app/Contents/SystemFiles/Links/WSTP/DeveloperKit
		ifeq ($(UNAME_P),x86_64)
			DEFAULT_SYS := MaxOSX-X86-64
		endif
		ifneq ($(filter arm%,$(UNAME_P)),)
			DEFAULT_SYS := MacOSX-ARM64
		endif
    endif
endif

MLINKDIR ?= ${DEFAULT_MLINKDIR}
SYS ?= ${DEFAULT_SYS}
CADDSDIR := ${MLINKDIR}/${SYS}/CompilerAdditions
MPREP = ${CADDSDIR}/wsprep

CXXFLAGS = -std=c++20 -stdlib=libstdc++ -stdlib=libc++ -stdlib=libc++ -Wall -I${CADDSDIR}
LDFLAGS = -L${CADDSDIR} -lWSTPi4 -lm -framework Foundation

SRC = $(wildcard *.cc)
HEADERS = $(wildcard *.h)

ifeq ($(RELEASE), true)
	CXXFLAGS += -O2
else
	CXXFLAGS += -g -ggdb -DPPERM_DEBUG -fsanitize=address
	LDFLAGS += -fsanitize=address
endif

all: pperm pperm-ml tests

pperm-ml: perm.cc.o mathlink.cc.o
	$(CXX) -o $@ $(LDFLAGS) $^

pperm: pperm.cc.0 main.cc.o
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
