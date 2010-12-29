%% EBOT, an erlang web crawler.
%% Copyright (C) 2010 ~ matteo DOT redaelli AT libero DOT it
%%                      http://www.redaelli.org/matteo/
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
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

command(_Method, "help", _Tokens, _ReqData) ->
    "Options:\n" ++
	"- crawler\n" ++
	"- help\n" ++
	"- ping\n" ++
	"- stats\n" ++
	"- worker\n"
	;
command(_Method, "ping", _Tokens, _ReqData) ->
    "pong";

%% urls must be at least
%% /.+/.+ 

command(_Method, "crawler", [Command|_Tokens], ReqData) ->
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
		    ebot_crawler:add_new_url( list_to_binary(Url) ),
		    Result = "Added url: " ++ Url;
		false ->
		    Result = "Invalid/Missing parameter url"
	    end;
	%% "analyze_url" ->
	%%     Url = wrq:get_qs_value("url",ReqData),
	%%     case is_list(Url) of
	%% 	true ->
	%% 	    ebot_web:analyze_url( list_to_binary(Url) ),
	%% 	    Result = "Url Analyzer has started for " ++ Url;
	%% 	false ->
	%% 	    Result = "Invalid/Missing parameter 'url'"
	%%     end;
   	"start" ->
    	    ebot_crawler:start_workers(),
	    Result = "Crawler Started!";
   	"stop" ->
	    TotH1 = length(ebot_html:get_workers()),
	    TotW1 = length(ebot_web:get_workers()),
    	    ebot_crawler:stop_workers(),
	    Result = "Stopped crawler! Stopping workers: html=" ++ 
		integer_to_list(TotH1) ++
		", web=" ++
		integer_to_list(TotW1);
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
    Result;

command(_Method, "worker", [Worker,Command|_Tokens], ReqData) ->
    %% TODO : check if worker is valid
    Module = list_to_atom("ebot_" ++ Worker),
    case Command of
    	"check_recover" ->
	    Before = Module:get_workers(),
    	    {Type, After} = Module:check_recover_workers(),
	    Recovered = length(lists:subtract(After, Before)),
	    Result = atom_to_list(Type) ++ " workers: restarted " ++ 
		integer_to_list(Recovered) ++
		"\n";
    	"start" ->
	    Depth = wrq:get_qs_value("depth", ReqData),
	    Tot = wrq:get_qs_value("tot", ReqData),
	    case (is_list(Depth) andalso is_list(Tot)) of
		true ->
		    Tot1 = length(ebot_web:get_workers()),
		    Module:start_workers(list_to_integer(Depth), list_to_integer(Tot)),
		    Tot2 = length(Module:get_workers()),
		    Result = "Starting workers: from " ++ 
			integer_to_list(Tot1) ++
			" to " ++
			integer_to_list(Tot2);
		false ->
		    Result = "Invalid/Missing parameters 'depth' and 'tot'"
	    end;
    	Else ->
    	    Result = Else
    end,
    Result;

command(Method, Command, _, _ReqData) ->
    io_lib:format("Unknow option: method=~ts, command=~ts", [Method, Command]).
