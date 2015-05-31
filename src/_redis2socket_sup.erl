-module(redis2socket_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, 
    	{ 
    		{one_for_one, 5, 10}, 
    		[
    			{                   
					redis_sup,     % - Register it under the name redis_sup
					{              % - Here's how to find and start this child's code 
					  redis_sup,   %   * the module is called redis_sup
					  start_link,  %   * the function to invoke is called start_link
					  []           %   * and here's the list of default parameters to use
					},                
					permanent,     % - child should run permantenly, restart on crash 
					2000,          % - give child 2 sec to clean up on system stop, then kill 
					supervisor,    % - FYI, this child is a supervisor, not a worker
					[redis_sup]    % - these are the modules the process uses  
				}
    		]
    	}
    }.
