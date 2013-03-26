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


flow.tex: Flow.lhs
	lhs2TeX -o $@ $<

custom_bench.tex: CustomBench.lhs
	lhs2TeX -o $@ $<

main.tex: Main.lhs
	lhs2TeX -o $@ $<

mylhs.sty: mylhs.lhs
	lhs2TeX -o $@ $<

minidoc.pdf: minidoc.tex flow.tex mylhs.sty
	pdflatex minidoc.tex

clean:
	@rm -f *.class *.hi *.o bench main.tex bench.pdf *.java
