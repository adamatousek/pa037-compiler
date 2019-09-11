.PHONY: test FORCE

llvmFLAGS = $(shell llvm-config-7 --cflags)
llvmLDFLAGS = $(shell llvm-config-7 --ldflags)
llvmLIBS = $(shell llvm-config-7 --libs)
DEMO_FILES = $(wildcard demo/*.glum)

SEAGOL_DEFINES = -DSEAGOL_RELAX_WARNINGS="_Pragma( \"GCC diagnostic push\" ) \
     _Pragma( \"GCC diagnostic ignored \\\"-Wold-style-cast\\\"\" ) \
     _Pragma( \"GCC diagnostic ignored \\\"-Wunused-parameter\\\"\" )" \
     -DSEAGOL_UNRELAX_WARNINGS="_Pragma( \"GCC diagnostic pop\" )"

CXXFLAGS += -std=c++14 -g -Wall -Wextra -pedantic $(llvmFLAGS) $(SEAGOL_DEFINES)
LDFLAGS += $(llvmLDFLAGS)

seagolc: parser.o lexer.o main.o compiler.o context.o
	$(CXX) $(LDFLAGS) -o $@ $^ $(llvmLIBS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

lexer.cpp: lexer.l lexer.hpp parser.hpp
	flex -o $@ $<

parser.cpp: parser.y compiler.hpp semantic.hpp
	bison $(YACCFLAGS) --defines=$(<:.y=.hpp) -o $@ $<


test: $(DEMO_FILES:=.o)

$(DEMO_FILES:=.o): %.o: % FORCE
	ls seagolc || $(MAKE) seagolc
	./seagolc $*

FORCE:

# Disable this implicit rule:
%: %.o
