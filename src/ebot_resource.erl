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
   	"crawlers" ->
    	    Result = command_crawlers(Tokens);
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


command_crawlers([]) ->
    "Unknow option for crawlers";
command_crawlers([Command|_Tokens]) ->
    case Command of
    	"check_recover" ->
	    Tot1 = length(ebot_web:show_crawlers_list()),
    	    ebot_web:check_recover_crawlers(),
	    Tot2 = length(ebot_web:show_crawlers_list()),
	    Result = "Crawlers: from " ++ 
		integer_to_list(Tot1) ++
		" to " ++
		integer_to_list(Tot2);
    	"start" ->
	    Tot1 = length(ebot_web:show_crawlers_list()),
    	    ebot_web:start_crawlers(),
	    Tot2 = length(ebot_web:show_crawlers_list()),
	    Result = "Starting crawlers: from " ++ 
		integer_to_list(Tot1) ++
		" to " ++
		integer_to_list(Tot2);
   	"stop" ->
	    Tot1 = length(ebot_web:show_crawlers_list()),
    	    ebot_web:stop_crawlers(),
	    Result = "Stopping " ++ 
		integer_to_list(Tot1) ++
		" crawlers";
    	_ ->
    	    Result = command_stats([])
    end,
    Result.


