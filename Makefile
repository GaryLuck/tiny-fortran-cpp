CXX      = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -pedantic
SRCDIR   = src
OBJDIR   = build
TARGET   = tinyfortran

SRCS = $(SRCDIR)/lexer.cpp $(SRCDIR)/ast.cpp $(SRCDIR)/parser.cpp \
       $(SRCDIR)/interpreter.cpp $(SRCDIR)/main.cpp
OBJS = $(patsubst $(SRCDIR)/%.cpp,$(OBJDIR)/%.o,$(SRCS))

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp | $(OBJDIR)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

clean:
	rm -rf $(OBJDIR) $(TARGET)

.PHONY: all clean
