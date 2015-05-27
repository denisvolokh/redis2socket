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

-record(state, {sub, ws_handler}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, WSHandlerPid) ->
	lager:info("[+ RH] Starting REDIS HANDLER with Name: ~p ~p", [Name, WSHandlerPid]),
    gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [Name, WSHandlerPid], []).
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
	[Name,WSHandlerPid] = Args,
	gproc:reg({n, l, Name}),
    
	lager:info("[+ RH] Connecting to Redis ... ~p ~p", [Name, WSHandlerPid]),

	case eredis_sub:start_link("localhost", 6379, "") of 
		{ok, Sub} ->
			lager:info("[+ RH] ... ~p connected!", [Sub]),

            eredis_sub:controlling_process(Sub, self()),

            % Channel = atom_to_list(Name),
            Channel = Name,
            lager:info("[+ RH] Subscribing for channel: ~p", [Channel]),            
            R = eredis_sub:subscribe(Sub, [Channel]),    

            {ok, #state{sub = Sub, ws_handler = WSHandlerPid}};
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
handle_info({new_subscriber_arrived, Pid}, State) -> 
    io:format("[+] New Subscriber: ~p~n",[Pid]),
    % Save subscribers
    {noreply, State};

handle_info({subscriber_left, Pid}, State) -> 
    io:format("[+] New Subscriber: ~p~n",[Pid]),
    % Save subscribers
    {noreply, State};

handle_info({subscribed, Channel, _}, State) -> 
    io:format("[+] Subscribed: ~p~n",[Channel]),
	eredis_sub:ack_message(State#state.sub),
    {noreply, State};

handle_info({message, Channel, Message, _}, State) -> 
    io:format("[+] Received: ~p ~p ~n",[Channel, Message]),
	eredis_sub:ack_message(State#state.sub),

	io:format("[+] Sending back to ws: ~p ~p ~n",[State#state.ws_handler, Message]),
	State#state.ws_handler ! {message, Message},
    {noreply, State}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.  

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
