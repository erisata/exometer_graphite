#/--------------------------------------------------------------------
#| Copyright 2017 Erisata, UAB (Ltd.)
#|
#| Licensed under the Apache License, Version 2.0 (the "License");
#| you may not use this file except in compliance with the License.
#| You may obtain a copy of the License at
#|
#|     http://www.apache.org/licenses/LICENSE-2.0
#|
#| Unless required by applicable law or agreed to in writing, software
#| distributed under the License is distributed on an "AS IS" BASIS,
#| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#| See the License for the specific language governing permissions and
#| limitations under the License.
#\--------------------------------------------------------------------

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

itest: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys' $(REBAR) ct skip_deps=true $(CT_ARGS) || grep Testing logs/raw.log

doc:
	$(REBAR) doc

clean: clean-itest
	$(REBAR) clean

clean-all: clean-itest
	$(REBAR) clean --recursive

.PHONY: all deps compile compile-all check test itest doc clean clean-all clean-itest


