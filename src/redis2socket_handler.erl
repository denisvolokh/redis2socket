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
    lager:info("[+] HANDLE ... ~p ~p", [Msg, Req]),

    MessageItems = re:split(Msg, ",", [{return, list}]),
    [Command | [Channel | _]] = MessageItems,
    case Command of 
        "SUBSCRIBE" ->
            lager:info("[+ SH] Subscribing for channels: ~p", [Channel]),
            
            lager:info("[+ SH] If does already exist ~p? ~p", [Channel, gproc:where({n, l, Channel})]),    
            case gproc:where({n, l, Channel}) of 
                undefined -> 
                    supervisor:start_child(redis_sup, [Channel, self()]);
                ExistingRedisHandlerPid ->
                    lager:info("[+ SH] Found: ~p", [ExistingRedisHandlerPid]),
                    ExistingRedisHandlerPid ! {new_subscriber_arrived, self()}
            end;

        "UNSUBSCRIBE" ->    
            lager:info("[+] UnSubscribing from channels: ~p", [Channel]);
        _ ->
            lager:info("[+] Unknown command: ~p", [Command])
    end,

    {reply, {text, << "responding to ", Msg/binary >>}, Req, State, hibernate };


websocket_handle(_Any, Req, State) ->
    lager:info("[+ SH] HANDLE ..."),
    {reply, {text, << "whut?">>}, Req, State, hibernate }.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    lager:info("[+] INFO ... ~p", [{timeout, _Ref, Msg}]),
    {reply, {text, Msg}, Req, State};

websocket_info({message, Msg}, Req, State) ->
    lager:info("[+ SH] Message from Redis ... ~p", [Msg]),
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
