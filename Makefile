.PHONY: all clean docs

all:
	dune build

clean:
	dune clean
	rm -rf docs

docs:
	dune build @doc
	mkdir docs
	cp -R _build/default/_doc/* docs
