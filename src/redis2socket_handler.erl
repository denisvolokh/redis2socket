-module(redis2socket_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/2, init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


% http://stackoverflow.com/questions/16908898/cowboy-websocket-global-processing

handle(Req, State) ->
    lager:debug("Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.


websocket_init(_TransportName, Req, _Opts) ->
    lager:info("[+] WEBSOCKET INIT ..."),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    lager:info("[+] HANDLE ... ~p", [Msg]),

    lager:info("[+] WHERE IS ~p ? ~p", [channel_name, gproc:where({n, l, channel_name})]),
    case gproc:where({n, l, channel_name}) of 
        undefined -> 
            supervisor:start_child(redis_sup, [channel_name, self()]);
        ExistingRedisHandlerPid ->
            lager:info("[+] Found: ~p", [ExistingRedisHandlerPid]),
            ExistingRedisHandlerPid ! {new_subscriber_arrived, self()}
    end,

    {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };


websocket_handle(_Any, Req, State) ->
    lager:info("[+] HANDLE ..."),
    {reply, {text, << "whut?">>}, Req, State, hibernate }.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    lager:info("[+] INFO ... ~p", [{timeout, _Ref, Msg}]),
    {reply, {text, Msg}, Req, State};

websocket_info({message, Msg}, Req, State) ->
    lager:info("[+] Message from Redis ... ~p", [Msg]),
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    lager:info("[+] INFO ... ~p", [_Info]),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    % send message to redis_handler process that this socket connection is closed
    lager:info("[+] Elvis has left the building! ~p", [_Req]),
    ok.

terminate(_Reason, _Req, _State) ->
    ok.
