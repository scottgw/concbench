.PHONY=all

GHC_OPTS=-rtsopts -threaded -O2 -Wall -fno-warn-orphans

all: bench

bench: Main.lhs Bench.lhs Java.lhs Dsl.lhs Chiz.hs
	ghc ${GHC_OPTS} $< -o $@

chart: ChizChart.hs
	ghc ${GHC_OPTS} $< -o $@ --make -main-is ChizChart

cache: Cache.hs
	ghc ${GHC_OPTS} $< -o $@

bench.pdf: bench.tex
	pdflatex bench.tex


dsl.tex: Flow.lhs
	lhs2TeX -o $@ $<

main.tex: Main.lhs
	lhs2TeX -o $@ $<

clean:
	@rm -f *.class *.hi *.o bench main.tex bench.pdf *.java
