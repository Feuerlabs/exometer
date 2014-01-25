.PHONY: all clean clean_plt deps compile test doc dialyzer xref devnode_snmp_agent devnode_snmp_manager compile_examples ci

EXOMETER_PLT=exometer.plt
DIALYZER_OPTS = -Wunderspecs
DIALYZER_APPS = erts kernel stdlib syntax_tools snmp \
		lager afunix netlink folsom exo

all: deps compile xref test

ci: compile xref test

deps:
	rebar get-deps

compile:
	rebar compile

clean: clean_plt
	rebar clean

test: compile_examples
	ERL_LIBS=./examples rebar ct skip_deps=true

xref:
	ERL_LIBS=./deps rebar xref skip_deps=true

edown_deps:
	rebar get-deps compile edown=true

doc: edown_deps
	rebar doc edown=true skip_deps=true

$(EXOMETER_PLT):
	rebar get-deps compile
	ERL_LIBS=deps dialyzer --build_plt --output_plt $(EXOMETER_PLT) \
	--apps $(DIALYZER_APPS)

clean_plt:
	rm -f $(EXOMETER_PLT)

dialyzer: $(EXOMETER_PLT)
	dialyzer -r ebin --plt $(EXOMETER_PLT) $(DIALYZER_OPTS)

compile_examples:
	erlc +'{parse_transform, lager_transform}' -pz deps/lager/ebin -I src -o examples/snmp_manager/ examples/snmp_manager/*.erl

devnode_snmp_agent:
	erl -sname agent -pa deps/*/ebin ebin -config examples/snmp_agent/sys.config -boot start_sasl -s lager -s crypto -s exometer

devnode_snmp_manager: compile_examples
	erl -sname manager -pz examples/snmp_manager -pz deps/*/ebin ebin -config examples/snmp_manager/sys.config \
		-boot start_sasl -s lager -s crypto -s snmp
