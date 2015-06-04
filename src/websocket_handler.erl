-module(websocket_handler).
% -behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

init(Req, Opts) ->
    lager:info("[+] INIT ..."),
    {cowboy_websocket, Req, Opts}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("[+] WEBSOCKET_INIT ..."),
    {ok, Req, undefined_state, 60000, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    lager:info("[+] HANDLE ... ~p", [Msg]),

    MessageItems = re:split(Msg, ",", [{return, list}]),
    
    [Command | Channels] = MessageItems,
    
    case Command of 
        "SUBSCRIBE" ->
            
            lager:info("[+] Handle SUBSCRIBE command: ~p", [Channels]),            
            spawn_listener(Channels);

        "UNSUBSCRIBE" ->    

            lager:info("[+] UnSubscribing from channels: ~p", [Channels]),
            unsubscribe(Channels);    
            
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------    

unsubscribe([]) -> ok;
unsubscribe([Channel | Channels]) ->
    ExistingProcess = gproc:where({n, l, Channel}),

    case ExistingProcess of 
        undefined -> 
            lager:info("[+ SH] Could not find Redis Handler process for channel ~p", Channel);
        _ ->
            lager:info("[+ SH] Found: ~p", [ExistingProcess]),
            ExistingProcess ! {websocket_unsubscribe, self()}
    end,

    unsubscribe(Channels).


spawn_listener([]) -> ok;
spawn_listener([Channel | Channels]) ->
    lager:info("[+ SH] Spawn Redis Listener process for channel: ~p", [Channel]),
    
    ExistingProcess = gproc:where({n, l, Channel}),

    lager:info("[+ SH] If does already exist ~p? ~p", [Channel, ExistingProcess]),    
    
    case ExistingProcess of 
        undefined -> 
            supervisor:start_child(redis_sup, [Channel, self()]);
        _ ->
            lager:info("[+ SH] Found: ~p", [ExistingProcess]),
            ExistingProcess ! {websocket_subscribe, self()}
    end,

    spawn_listener(Channels).

