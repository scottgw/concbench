main: main.c
	gcc -std=gnu99 -O0 $< -o $@ `pkg-config libqs --cflags --libs`

clean:
	@rm -f main
