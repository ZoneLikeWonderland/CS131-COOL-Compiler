SUPPORTDIR= ../cool-support
LIB =
 
SRC= cgen.cc cgen.h cgen_supp.cc cool-tree.h cool-tree.handcode.h emit.h 
CSRC= cgen-phase.cc utilities.cc stringtab.cc dumptype.cc tree.cc cool-tree.cc ast-lex.cc ast-parse.cc handle_flags.cc 
CGEN=
CFILES=  cgen.cc cgen_supp.cc $(CGEN) ${CSRC}   
OBJS= ${CFILES:.cc=.o}
 
CFLAGS=-g -Wall -Wno-unused ${CPPINCLUDE} -DDEBUG
CPPINCLUDE= -I. -I${SUPPORTDIR}/include  -I${SUPPORTDIR}/src
CC= g++

all: cgen


cgen: ${OBJS}  
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o cgen
 
.cc.o:
	${CC} ${CFLAGS} -c $<

${CSRC}:
	-ln -s ${SUPPORTDIR}/src/$@ $@   

clean :
	-rm -f core *.s ${CSRC} cgen *.o *.a

 

%.d: %.cc ${SRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}
