all: src

src:
	rebar compile xref

clean:
	rebar clean

.PHONY: clean src
