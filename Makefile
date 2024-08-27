.PHONY: all clean

all: aback core


aback:
	dune build

core: install
	cd core && make


install: aback core
	dune install


clean:
	dune clean
	cd core && make clean
