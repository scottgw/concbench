.PHONY=all

GHC_OPTS=-rtsopts -threaded -O2 -Wall

all: benchdsl

benchdsl: BenchDSL.lhs Bench.lhs Java.lhs Dsl.lhs
	ghc ${GHC_OPTS} $< -o $@

cache: Cache.hs
	ghc ${GHC_OPTS} $< -o $@

bench.pdf: bench.tex
	pdflatex bench.tex

bench.tex: BenchDSL.lhs
	lhs2TeX -o bench.tex BenchDSL.lhs
