REBAR=rebar

all: compile-all

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

compile-all:
	$(REBAR) compile --recursive

check: test

test: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys -s lager' $(REBAR) eunit skip_deps=true verbose=1

doc:
	$(REBAR) doc

clean: clean-itest
	$(REBAR) clean

clean-all: clean-itest
	$(REBAR) clean --recursive

.PHONY: all deps compile compile-all check test itest doc clean clean-all clean-itest


