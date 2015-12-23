-module(redis_handler).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sub, sockets}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, SocketHandlerPid) ->
	lager:info("[+ RH] Starting REDIS HANDLER with Name: ~p ~p", [Name, SocketHandlerPid]),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [Name, SocketHandlerPid], []).
    % gen_server:start_link({local, test}, ?MODULE, [Name, WSHandlerPid], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Stopping server asynchronously
stop() ->
    gen_server:cast(?MODULE, shutdown).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	[Name, SocketHandlerPid] = Args,
	gproc:reg({n, l, Name}),
    
	lager:info("[+ RH] Connecting to Redis ... ~p ~p", [Name, SocketHandlerPid]),

    RedisHostAddress = case os:getenv("REDIS_PORT_6379_TCP_ADDR") of false -> "0.0.0.0"; Any -> Any end,
    lager:info("[+ RH] Redis Host Found: ~p", [RedisHostAddress]),

    case eredis_sub:start_link(RedisHostAddress, 6379, "") of 
        {ok, Sub} ->
            lager:info("[+ RH] ... ~p connected!", [Sub]),

            eredis_sub:controlling_process(Sub, self()),

            % Channel = atom_to_list(Name),
            Channel = Name,
            lager:info("[+ RH] Subscribing for channel: ~p", [Channel]),            
            R = eredis_sub:subscribe(Sub, [Channel]),    

            {ok, #state{sub = Sub, sockets = [SocketHandlerPid]}};
        {error, Error} ->
            lager:info("[- RH] Unable connect to Redis: ~p ...", [Error]),
            {ok, #state{}}
    end.

%% Synchronous, possible return values  
% {reply,Reply,NewState} 
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState} 
% {stop,Reason,NewState}
handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};

%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info({websocket_subscribe, Pid}, State) -> 
    lager:info("[+] New Subscriber: ~p~n",[Pid]),
    
    case lists:member(Pid, State#state.sockets) of 
        false ->
            NewSubscribers = State#state.sockets ++ [Pid],
            lager:info("[+ RH] Subscribers ~p !", [NewSubscribers]),
            {noreply, #state{sub = State#state.sub, sockets = NewSubscribers}};

        true ->
            lager:info("[+ RH] ~p is already in the list (~p)!", [Pid, State#state.sockets]),
            {noreply, State}
    end;

handle_info({websocket_unsubscribe, Pid}, State) -> 
    lager:info("[+ RH] UnSubscribe: ~p~n",[Pid]),
    
    case lists:member(Pid, State#state.sockets) of 
        true ->
            LeftSubscribers = lists:delete(Pid, State#state.sockets),

            case length(LeftSubscribers) of 
                0 ->
                    lager:info("[+ RH] No subscribers left, terminating Redis Handler!"),
                    {stop, normal, State};
                _ ->
                    {noreply, #state{sub = State#state.sub, sockets = LeftSubscribers}}    
            end;

        false ->
            {noreply, State}
    end;

handle_info({subscribed, Channel, _}, State) -> 
    lager:info("[+ RH] Subscribed: ~p~n",[Channel]),
	eredis_sub:ack_message(State#state.sub),
    {noreply, State};

handle_info({message, Channel, Message, _}, State) -> 
    lager:info("[+ RH] Received from redis: ~p ~p ~n",[Channel, Message]),
	eredis_sub:ack_message(State#state.sub),

    send_message(State#state.sockets, Message),

    {noreply, State}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.  

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_message([], Message) ->
    ok;

send_message([Socket|Sockets], Message) ->
    lager:info("[+ RH] Send message to socket: ~p ~p", [Socket, Message]),
    Socket ! {message, Message},
    send_message(Sockets, Message).

