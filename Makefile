# Makefile for building the parser and interpreter

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.
.PHONY : all clean

# Default goal.
all : interpreter

# Rules for building the parser.
AbsAJ.hs LexAJ.x ParAJ.y PrintAJ.hs TestAJ.hs ErrM.hs : ./AJ.cf
	bnfc --haskell --functor ./AJ.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestAJ :  LexAJ.hs ParAJ.hs PrintAJ.hs TestAJ.hs
	${GHC} ${GHC_OPTS} $@

# Rules for building the interpreter.
Interpreter : Interpreter.hs
	${GHC} ${GHC_OPTS} $@

interpreter : Main.hs Interpreter.hs ErrM.hs LexAJ.hs ParAJ.hs PrintAJ.hs AbsAJ.hs
	${GHC} --make $< -o $@

# Rules for cleaning generated files.
clean :
	-rm -f *.hi *.o *.log *.aux *.dvi *.info *.y *.txt *.bak *.x AbsAJ.hs ErrM.hs LexAJ.hs ParAJ.hs PrintAJ.hs SkelAJ.hs TestAJ.hs TestAJ interpreter

# EOF
