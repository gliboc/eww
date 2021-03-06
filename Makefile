REBAR=./rebar3



all: rebar3 
	$(REBAR) compile

rebar3 : 
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

run:
	$(REBAR) shell

run_old:
	erl -pa _build/default/lib/eww/ebin/ -pa _build/default/lib/uuid/ebin/

docs:
	rm -rf ./doc
	$(REBAR) edoc
	$(BROWSER) ./doc/overview-summary.html

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
