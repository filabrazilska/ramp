all:
	rebar compile

test:
	rebar eunit

clean:
	rebar clean

.PHONY: all test clean
