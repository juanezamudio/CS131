interp : Main.hs Eval.hs Check.hs Test.hs Syntax.hs
	ghc -o $@ $^

test : interp tests/*
	for t in tests/*; do ./interp $$t; done

clean :
	rm interp *.hi *.o
