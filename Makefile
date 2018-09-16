athena: Makefile athena.cabal stack.yaml src/*.hs
	stack build

test:
	stack test

test-inference:
	stack test athena:test-infer

test-parser:
	stack test athena:test-parser

run:
	stack build
	stack exec athena -- examples/fib.at

.PHONY: run athena
