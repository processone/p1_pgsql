REBAR ?= rebar

all: src

src:
	$(REBAR) compile
	$(REBAR) xref

clean:
	$(REBAR) clean

.PHONY: clean src
