.PHONY: all compile rel run test clean
.PHONY: dialyzer

REBAR=./rebar3

all: compile

deps:
	        $(REBAR) install_deps

compile: deps
		$(REBAR) compile

test:
		$(REBAR) eunit skip_deps=true verbose=3

clean:
		$(REBAR) clean
		rm -rf ./log
		rm -rf ./erl_crash.dump

dialyzer:
		$(REBAR) dialyzer
