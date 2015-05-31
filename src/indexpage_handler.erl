-module(indexpage_handler).

-export([init/3, init/2]).
-export([handle/2]).
-export([terminate/3]).

init(Req, _Opts) ->
    {ok, Req, undefined}.

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Html = get_html(),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html() ->
    {ok, Cwd} = file:get_cwd(),
    Filename =filename:join([Cwd, "priv", "index.html"]),
    case filelib:is_file(Filename) of 
        true ->
            {ok, Binary} = file:read_file(Filename),
            Binary;
        false ->
            case code:lib_dir(devo) of
                {error, _Reason} ->
                    throw({error, "I could not find where Index Page is installed."});
                LibDir ->
                    Filename1 =filename:join([LibDir, "priv", "index.html"]),
                    {ok, Binary} = file:read_file(Filename1),
                    Binary
            end
    end.