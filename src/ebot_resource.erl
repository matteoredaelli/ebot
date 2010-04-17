%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.
%% @doc Example webmachine_resource.

-module(ebot_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {"<html><body>Hello, new world</body></html>", ReqData, State}.
