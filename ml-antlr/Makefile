#
# COPYRIGHT (c) 2006
# John Reppy (http://www.cs.uchicago.edu/~jhr)
# Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
# All rights reserved.
#

OS =		$(shell uname -s)

ifeq ($(shell uname -s),Darwin)
HEAP_SUFFIX =		ppc-darwin
else
HEAP_SUFFIX =		x86-linux
endif

SHELL =			/bin/sh
SML =			sml
ML_BUILD =		ml-build
ML_MAKEDEPEND =		ml-makedepend

PROGRAM =	ml-antlr
HEAP_IMAGE =	$(PROGRAM).$(HEAP_SUFFIX)

FE = 		FrontEnds
BE = 		BackEnds

SML_SRC = 	$(wildcard $(BE)/SML/*.sml)
LATEX_SRC =     $(wildcard $(BE)/LaTeX/*.sml)

FE_SRC = 	$(wildcard $(FE)/*.sml)  $(FE)/ml-yacc-based/spec.grm $(FE)/ml-yacc-based/spec.lex
BE_SRC = 	$(wildcard $(BE)/*.sml) $(SML_SRC) $(LATEX_SRC)

TEMPLATES =     $(BE)/SML/template.sml $(BE)/LaTeX/template.tex

CORE_SRC =	$(wildcard *.sml)

SOURCES =	$(CORE_SRC) $(FE_SRC) $(BE_SRC) $(TEMPLATES) sources.cm

build:		$(HEAP_IMAGE)

$(HEAP_IMAGE):	$(SOURCES)
	$(ML_BUILD) sources.cm Main.main $(PROGRAM)

.depend:	$(CM_FILES)
	touch .depend
	$(ML_MAKEDEPEND) -n -f .depend sources.cm $(HEAP_IMAGE)

sinclude .depend

clean:
	rm -rf .depend .cm FrontEnds/ml-yacc-based/spec.lex.sml FrontEnds/ml-yacc-based/spec.grm.sig FrontEnds/ml-yacc-based/spec.grm.sml $(HEAP_IMAGE)

devclean:
	rm -rf $(HEAP_IMAGE)