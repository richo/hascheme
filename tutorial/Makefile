all: ex1 ex2

.PHONY: clean

%: %.hs
	ghc -o $* --make $^

clean:
	rm *.hi
	rm *.o
