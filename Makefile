CXXFLAGS += -g

seagolc: parser.o lexer.o main.o compiler.o
	$(CXX) $(CXXFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

lexer.cpp: lexer.l lexer.hpp
	flex -o $@ $<

parser.cpp: parser.y lexer.hpp compiler.hpp
	bison --defines=$(<:.y=.hpp) -o $@ $<
