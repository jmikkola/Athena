athena: Makefile athena.cabal stack.yaml src/*.hs test/*.hs
	stack build
	stack test

run:
	stack build
	stack exec athena -- examples/mvp.at

.PHONY: run athena
