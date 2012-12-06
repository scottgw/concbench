WARN=-Wall -fno-warn-unused-do-bind
OUTPUT=-hidir tmp/ -odir tmp/
GHCOPTS=-O2 -threaded -rtsopts -funbox-strict-fields ${WARN} ${OUTPUT}

anneal: Anneal.hs City.hs State.hs tmp
	ghc --make Anneal.hs -o anneal ${GHCOPTS}

tmp:
	mkdir tmp

clean:
	@rm -rf tmp/ anneal