all:
	cabal build
	cp dist/build/ver/ver .

clean:
	cabal clean
	rm ver
	rm src/.*.swp
