%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.

%% @doc TEMPLATE.

-module(ebot).
-author('author <matteo.redaelli@libero.it>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the ebot server.
start() ->
    ebot_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    ensure_started(couchbeam),
    application:start(ebot).

%% @spec stop() -> ok
%% @doc Stop the ebot server.
stop() ->
    Res = application:stop(ebot),
    application:stop(webmachine),
    application:stop(couchbeam),
    application:stop(crypto),
    Res.
