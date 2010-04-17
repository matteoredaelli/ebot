%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.

%% @doc Callbacks for the ebot application.

-module(ebot_app).
-author('author <matteo.redaelli@libero.it>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ebot.
start(_Type, _StartArgs) ->
    ebot_deps:ensure(),
    ebot_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ebot.
stop(_State) ->
    ok.
