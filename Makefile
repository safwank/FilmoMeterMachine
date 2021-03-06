ERL ?= erl
APP := filmometer

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@./rebar skip_deps=true eunit
