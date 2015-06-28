athena: Makefile athena.cabal stack.yaml src/*.hs test/*.hs
	stack build
	stack test

run:
	stack build
	stack exec athena

.PHONY: run athena
