CC=clang

SOURCES=$(wildcard *.m)
OBJECTS=$(SOURCES:.m=.o)

libtest.a : $(OBJECTS)
	$(AR) rcs libtest.a $(OBJECTS)
	ranlib libtest.a

%.c.o : %.c $(C_DEPS)
	$(VERBOSE)$(CC) $(ALL_CFLAGS) -o $@ -c $<
	 
%.cpp.o : %.cpp $(C_DEPS)
	$(VERBOSE)$(CXX) $(CXXFLAGS) -o $@ -c $<
	 
%.m.o : %.m $(C_DEPS)
	$(VERBOSE)$(CXX) $(ALL_CFLAGS) -o $@ -c $<
	 
%.mm.o : %.mm $(C_DEPS)
	$(VERBOSE)$(CXX) $(CXXFLAGS) -o $@ -c $< 
