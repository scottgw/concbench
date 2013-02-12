WARN=-Wall -fno-warn-unused-do-bind
OUTPUT=-hidir tmp/ -odir tmp/
GHCOPTS=-O2 -threaded -rtsopts -funbox-strict-fields ${WARN} ${OUTPUT}

anneal: Anneal.hs City.hs State.hs tmp
	ghc --make Anneal.hs -o anneal ${GHCOPTS}

annealp: Anneal.hs City.hs State.hs tmp
	ghc --make Anneal.hs -o annealp ${GHCOPTS} -rtsopts -prof -auto-all

inputgen: Generator.hs City.hs tmp
	ghc --make Generator.hs -o inputgen ${GHCOPTS}

anneal.in: inputgen
	./inputgen 1000 200

tmp:
	mkdir tmp

clean:
	@rm -rf tmp/ anneal
