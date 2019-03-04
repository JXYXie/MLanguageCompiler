all:
	happy -gca ParM.y
	alex -g LexM.x
	ghc --make Main.hs -o Main

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocM.* LexM.* ParM.* LayoutM.* SkelM.* PrintM.* TestM.* AbsM.* TestM ErrM.* SharedString.* ComposOp.* M.dtd XMLM.* Makefile*
