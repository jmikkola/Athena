athena: Makefile athena.cabal stack.yaml src/*.hs
	stack build

lint:
	hlint src/ --ignore "Reduce duplication"

test: lint
	stack test

test-inference:
	stack test athena:test-infer

test-parser:
	stack test athena:test-parser

test-first-pass:
	stack test athena:test-first-pass

test-graph:
	stack test athena:test-graph

run:
	stack build
	stack exec athena -- examples/fib.at

.PHONY: run athena
