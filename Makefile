all: hscm

hscm: src/hascheme.hs
	ghc -o hscm --make $^
