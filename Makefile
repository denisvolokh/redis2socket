REBAR = ./rebar

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa ebin -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/goldrush/ebin -pa deps/lager/ebin -pa deps/ranch/ebin -pa deps/eredis/ebin deps/gproc/ebin -s redis2socket_app )

.PHONY: all deps compile clean run