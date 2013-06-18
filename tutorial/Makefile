all: ex1 ex2 ex3 ex4 ex5

.PHONY: clean

%: %.hs
	ghc -o $* --make $^

clean:
	rm *.hi
	rm *.o
