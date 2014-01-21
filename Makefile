.PHONY: all clean clean_plt deps compile test doc dialyzer

EXOMETER_PLT=exometer.plt
DIALYZER_OPTS = -Wunderspecs

all: deps compile xref test

deps:
	rebar get-deps

compile: deps
	rebar compile

clean: clean_plt
	rebar clean

test:
	rebar eunit skip_deps=true

xref:
	ERL_LIBS=./deps rebar xref skip_deps=true

edown_deps:
	rebar get-deps compile edown=true

doc: edown_deps
	rebar doc edown=true skip_deps=true

$(EXOMETER_PLT):
	rebar get-deps compile
	ERL_LIBS=deps dialyzer --build_plt --output_plt $(EXOMETER_PLT) \
	--apps erts kernel stdlib -r deps

clean_plt:
	rm -f $(EXOMETER_PLT)

dialyzer: $(EXOMETER_PLT)
	dialyzer -r ebin --plt $(EXOMETER_PLT) $(DIALYZER_OPTS)