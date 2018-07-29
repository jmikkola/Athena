athena: Makefile athena.cabal stack.yaml src/*.hs
	stack build

test: athena
	stack exec athena -- --test
	stack test

run:
	stack build
	stack exec athena -- examples/mvp.at

.PHONY: run athena
