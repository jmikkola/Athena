./dist/build/athena/athena: Makefile athena.cabal stack.yaml src/*.hs test/*.hs
	stack build
	stack test

run: ./dist/build/athena/athena
	stack exec athena
