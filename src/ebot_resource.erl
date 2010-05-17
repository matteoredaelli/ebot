%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.
%% @doc Example webmachine_resource.

-module(ebot_resource).
-export([
	 init/1,
	 content_types_provided/2, 
	 to_text/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
   {[{"text/plain",to_text}], ReqData, State}.

to_text(ReqData, State) ->
    {ok, Command} = dict:find(command, wrq:path_info(ReqData)),
    Tokens = wrq:path_tokens(ReqData),
    case Command of
    	"ping" ->
    	    Result = "pong";
    	"stats" ->
    	    Result = command_stats(Tokens);
    	_ ->
    	    Result = "Unknown command"
    end,
    {Result, ReqData, State}.


command_stats([]) ->
    "Unknow option for stats";
command_stats([Command|_Tokens]) ->
    case Command of
	"show" ->
    	    Result = ebot_stats:statistics();
    	"update_rrd" ->
	    Path = filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "data"]),
    	    Result = ebot_stats:update_rrd_statistics(Path);
    	_ ->
    	    Result = command_stats([])
    end,
    Result.
    
