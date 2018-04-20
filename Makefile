REBAR=rebar3

all:
	$(REBAR) compile

run: 
	erl -pa _build/default/lib/eww/ebin/ -pa _build/default/lib/uuid/ebin/

docs:
	$(REBAR) edoc skip_deps=true

test: common_test

common_test:
	$(REBAR) ct

rel:
	$(REBAR) release

state:
	$(REBAR) release -d

dialyzer:
	$(REBAR) dialyzer

clean:
	$(REBAR) clean
