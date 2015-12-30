-module(redis_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	lager:info("[+] Starting Redis Handler Supervisor"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	
	{ok, {{simple_one_for_one, 0, 1},
          [{redis_handler, {redis_handler, start_link, []},
            temporary, brutal_kill, worker, [redis_handler]}]}}.


% http://blog.rusty.io/2009/09/16/g-proc-erlang-global-process-registry/

% http://stackoverflow.com/questions/4837196/erlang-supervisor3-adding-a-child-process