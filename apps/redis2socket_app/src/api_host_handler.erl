-module(api_host_handler).
% this is what a handler (or what i like to call them 'controller') behaviour
-behaviour(cowboy_http_handler).

%% Standard callbacks for the behaviour.
-export([init/3]).
-export([terminate/3]).
-export([handle/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

%% Custom callbacks for my app.
-export([handle_get/2]).
-export([handle_post/2]).

% This starts up every time someone invokes the endpoint, whether that is GET, POST and so on
init(_, _, _) ->
	io:format("init: got here!~p~n", ["d"]),
	{upgrade, protocol, cowboy_rest}.

% Another function where you can intercept the call in the pipeline
handle(Req, State) ->
    {ok, Req, State}.

% A function that gets called if the behaviour was terminated.
terminate(_Reason, _Req, _State) ->
    ok.

% This is a function that tells what methods are allowed for an end point
allowed_methods(Req, State) ->
	io:format("allowed_methods: got here!~p~n", ["d"]),
  	{[<<"GET">>, <<"POST">>], Req, State}.

% This is called on GET typically and calls a function based on the calls Content-Type
content_types_provided(Req, State) ->
	io:format("content_types_provided: got here!~p~n", ["d"]),
	{[
		{{<<"application">>,<<"json">>, []}, handle_get}
	], Req, State}.

% This is called on Post typically and calls a function based on the calls Accept
content_types_accepted(Req, State) ->
	io:format("content_types_accepted: got here!~p~n", ["d"]),
  	{[
  		{{<<"application">>, <<"json">>, []}, handle_post}
  	], Req, State}.

% This is what the GET call does.
% This is just returning a json object
handle_get(Req, State) ->
	io:format("Handle_get: got here!~p~n", ["d"]),
	% Body = jsx:encode([{<<"library">>,<<"jsx">>},{<<"awesome">>,true},{<<"IsAwesome">>,<<"ME">>}]),
	HostIpAddress = case os:getenv("HOST_IP_ADDRESS") of false -> <<"0.0.0.0">>; Ip -> list_to_binary(Ip) end,
	HostPort = case os:getenv("HOST_PORT") of false -> <<"8080">>; Port -> list_to_binary(Port) end,
	Body = jsx:encode([{<<"host_ip_address">>, HostIpAddress}, {<<"host_port">>, HostPort}]),
	{Body, Req, State}.

% This is what the POST call does.
% This is just returning a json object
handle_post(Req, State) ->
	io:format("Handle_post: got here!~p~n", ["d"]),
	%{ok, Body, Req2} = cowboy_req:body(Req),
	{ok, _, Req2} = cowboy_req:body(Req),
  	Body = jsx:encode([{<<"library">>,<<"derp">>},{<<"awesome">>,<<"nerp">>},{<<"IsAwesome">>,<<"ME">>}]),
  	Req3 = cowboy_req:set_resp_body(Body, Req2),
  	{true, Req3, State}.