-module(redis2socket_app).
-behaviour(application).
-export([start/0, start/2, stop/0, stop/1]).

-define(APPS, [compiler, syntax_tools, crypto, sasl, cowlib, ranch, cowboy, goldrush, lager, eredis, gproc, redis2socket]).

ensure_started([]) -> ok;
ensure_started([App|Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

start() ->
    lager:info("[+] Starting Applications ..."),
    ok = ensure_started(?APPS).

start(_StartType, _StartArgs) ->
    lager:info("[+] Starting Cowboy ..."),
    Dispatch = cowboy_router:compile([
            {'_', [
                {"/", cowboy_static, {file, "priv/index.html"}},
                {"/websocket", redis2socket_handler, []}
            ]}
        ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    redis2socket_sup:start_link().

% https://github.com/ninenines/cowboy/tree/master/examples/websocket

stop(_State) ->
    ok.

stop() ->
    ok = stop_apps(lists:reverse(?APPS)).

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).