.PHONY: all clean clean_plt deps compile test doc dialyzer xref devnode_snmp_agent devnode_snmp_manager compile_examples ci

REBAR3 ?= rebar3

all: compile xref test

ci: compile xref dialyzer test

compile:
	${REBAR3} compile

clean: clean_plt
	${REBAR3} clean

clean-all: clean
	rm -rf _build

test: compile_examples
	ERL_LIBS=./examples $(REBAR3) ct

xref:
	${REBAR3} as full do xref

doc:
	$(REBAR3) as docs do edoc

clean_plt:
	rm -f $(EXOMETER_PLT)

compile_examples:
	erlc -I src -o examples/snmp_manager/ examples/snmp_manager/*.erl

devnode_snmp_agent:
	erl -sname agent -pa deps/*/ebin ebin -config examples/snmp_agent/sys.config -boot start_sasl -s crypto -s exometer

devnode_snmp_manager: compile_examples
	erl -sname manager -pz examples/snmp_manager -pz deps/*/ebin ebin -config examples/snmp_manager/sys.config \
		-boot start_sasl -s crypto -s snmp
