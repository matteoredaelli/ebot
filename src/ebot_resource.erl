%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.
%% @doc Example webmachine_resource.

-module(ebot_resource).
-export([
	 init/1,
	 allowed_methods/2,
	 content_types_accepted/2, 
	 content_types_provided/2, 
	 to_json/2,
	 to_text/2
	]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'POST'], ReqData, State}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

content_types_provided(ReqData, State) ->
   {[{"text/plain",to_text},{"application/json", to_json}], ReqData, State}.

to_json(ReqData, State) ->
    {json_body(wrq:req_qs(ReqData)), ReqData, State}.

to_text(ReqData, State) ->
    {ok, Command} = dict:find(command, wrq:path_info(ReqData)),
    Tokens = wrq:path_tokens(ReqData),
    Method = wrq:method(ReqData),
    Result = command(Method, Command, Tokens, ReqData),
    {Result, ReqData, State}.

%% INTERNAL FUNCTIONS

json_body(QS) -> mochijson:encode({struct, QS}).

command(_Method, "ping", _Tokens, _ReqData) ->
    "pong";

%% urls must be at least
%% /.+/.+ 

command(Method, Command, [], _ReqData) ->
    io_lib:format("Unknow option: method=~ts, command=~ts", [Method, Command]);

command(_Method, "crawlers", [Command|_Tokens], ReqData) ->
    case Command of
	"add_url" ->
%	    case Method of
%		'POST' ->
%		    Result = "ok";
%		_Else ->
%		    Result = "Only POST request is supported"
%	    end;
	    Url = wrq:get_qs_value("url",ReqData),
	    case is_list(Url) of
		true ->
		    ebot_cache:add_new_url( list_to_binary(Url) ),
		    Result = "Added url: " ++ Url;
		false ->
		    Result = "Invalid/Missing parameter url"
	    end;
	"analyze_url" ->
	    Url = wrq:get_qs_value("url",ReqData),
	    case is_list(Url) of
		true ->
		    ebot_web:analyze_url( list_to_binary(Url) ),
		    Result = "Url Analyzer has started for " ++ Url;
		false ->
		    Result = "Invalid/Missing parameter url"
	    end;
    	"check_recover" ->
	    Before = ebot_web:show_crawlers_list(),
    	    After = ebot_web:check_recover_crawlers(),
	    Recovered = length(lists:subtract(After, Before)),
	    Result = "Crawlers: restarted " ++ 
		integer_to_list(Recovered) ++
		" crawlers\n";
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
    	Else ->
    	    Result = Else
    end,
    Result;

command(_Method, "stats", [Command|_Tokens], _ReqData) ->
    case Command of
	"show" ->
    	    Result = ebot_stats:statistics();
    	"update_rrd" ->
	    Path = filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "data"]),
    	    Result = ebot_stats:update_rrd_statistics(Path);
    	Else ->
    	    Result = Else
    end,
    Result.

