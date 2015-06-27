athena: Makefile athena.cabal src/*.hs test/*.hs
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test
