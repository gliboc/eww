all:
	rebar3 compile

run: 
	erl -pa _build/default/lib/eww/ebin/ -pa _build/default/lib/uuid/ebin/
