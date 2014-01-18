.PHONY: all clean compile test doc

all: compile xref test

compile:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit skip_deps=true

xref:
	ERL_LIBS=./deps rebar xref skip_deps=true

edown_deps:
	rebar get-deps compile edown=true

doc: edown_deps
	rebar doc edown=true skip_deps=true
