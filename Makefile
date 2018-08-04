athena: Makefile athena.cabal stack.yaml src/*.hs
	stack build

test:
	stack test

run:
	stack build
	stack exec athena -- examples/mvp.at

.PHONY: run athena
