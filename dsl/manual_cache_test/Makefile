matrix: Matrix.hs
	ghc -O2 $< -o $@

cache_local.txt: matrix
	valgrind --tool=cachegrind ./matrix local 2> $@

cache_nonlocal.txt: matrix
	valgrind --tool=cachegrind ./matrix nonlocal 2> $@
