.SILENT:

all: build

build:
	dune build
	cp -f ./_build/install/default/bin/DoomMetal ./

exec: build
	./DoomMetal 

clean:
	rm -f DoomMetal
	dune clean