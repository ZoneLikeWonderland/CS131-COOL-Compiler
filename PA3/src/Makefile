SUPPORT = ../cool-support
 
SRC= semant.cc semant.h cool-tree.h cool-tree.handcode.h 
CSRC= semant-phase.cc symtab_example.cc  handle_flags.cc  ast-lex.cc ast-parse.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc
CGEN= 
CFIL= semant.cc ${CSRC} ${CGEN} 
OBJS= ${CFIL:.cc=.o} 

all: semant

CPPINCLUDE= -I. -I${SUPPORT}/include -I${SUPPORT}/src



CC=g++
CFLAGS=-g -Wall -Wno-unused ${CPPINCLUDE} -DDEBUG

DEPEND = ${CC} -MM ${CPPINCLUDE}


SEMANT_OBJS := ${filter-out symtab_example.o,${OBJS}}

semant:  ${SEMANT_OBJS}  
	${CC} ${CFLAGS} ${SEMANT_OBJS} ${LIB} -o semant

symtab_example: symtab_example.cc 
	${CC} ${CFLAGS} symtab_example.cc ${LIB} -o symtab_example

.cc.o:
	${CC} ${CFLAGS} -c $<


# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.
 
 
${CSRC}:
	-ln -s ${SUPPORT}/src/$@ $@
 

clean :
	-rm -f  *.s core ${OBJS} ${CSRC} semant   symtab_example  *.d *~ *.a *.o
 

%.d: %.cc ${SRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}

