.PHONY: all clean docs

all:
	jbuilder build

clean:
	jbuilder clean
	rm -rf docs

docs:
	jbuilder build @doc
	mkdir docs
	cp -R _build/default/_doc/* docs
