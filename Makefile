.PHONY: all clean clean-all compile test doc dialyzer xref ci

REBAR3 ?= rebar3

all: compile xref test

ci: compile xref dialyzer test

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

clean-all: clean
	rm -rf _build

test: compile_examples
	$(REBAR3) do eunit
	$(REBAR3) do ct

xref:
	$(REBAR3) xref

doc:
	$(REBAR3) as docs do edoc

dialyzer: compile
	$(REBAR3) dialyzer

compile_examples:
	erlc +'{parse_transform, lager_transform}' -pz _build/default/lib/lager/ebin -I src -o examples/snmp_manager/ examples/snmp_manager/*.erl
