all:
	cabal install -fdevelopment && ./dist/build/positionsites/positionsites

run:
	./dist/build/positionsites/positionsites
