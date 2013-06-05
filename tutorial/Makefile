all: ex1

.PHONY: clean

%: %.hs
	ghc -o $* --make $^

clean:
	rm *.hi
	rm *.o
