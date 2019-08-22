CXXFLAGS += -g

seagolc: parser.o lexer.o main.o
	$(CXX) $(CXXFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

lexer.cpp: lexer.l lexer.hpp
	flex -o $@ $<

parser.cpp: parser.y lexer.hpp
	bison --defines=$(<:.y=.hpp) -o $@ $<
