.PHONY=all

all: benchdsl

benchdsl: BenchDSL.lhs
	ghc -O2 -threaded $< -o $@

cache: Cache.hs
	ghc -O2 -threaded $< -o $@


bench.pdf: bench.tex
	pdflatex bench.tex

bench.tex: BenchDSL.lhs
	lhs2TeX -o bench.tex BenchDSL.lhs

