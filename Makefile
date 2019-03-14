#############################################
# "Alias" makefile
#############################################

# Targets to run things:
.PHONY: build test test-verbose dialyzer shell clean purge

build:
	rebar3 compile

test:
	rebar3 ct

test-verbose:
	rebar3 ct --verbose

dialyzer:
	rebar3 dialyzer

shell:
	rebar3 shell

clean:
	rebar3 clean

purge:
	rm -rf _build/* _build ebin/* _ebin

