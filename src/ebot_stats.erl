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
%%%-------------------------------------------------------------------
%%% File    : ebot_stats.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created : 16 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_stats).

-define(RRDUPDATE, "rrdupdate").
-define(STATS_MODULES, [ebot_db, ebot_crawler, ebot_html, ebot_mq, ebot_web]).
%% API
-export([
	 statistics/0,
	 update_rrd_statistics/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

statistics() ->
    lists:foldl(
      fun(Mod, Output) ->
	      Stats = Mod:statistics(),
	      Output ++ "***********************\nModule " ++ 
		  atom_to_list(Mod) ++ 
		  "\n***********************\n" ++ stats_to_text(Stats) ++ "\n"
      end,
      "",
      ?STATS_MODULES).

update_rrd_statistics(Path) ->
    lists:foldl(
      fun(Mod, Output) ->
	      Stats = Mod:statistics(),
	      File = Path ++ "/" ++ atom_to_list(Mod) ++ ".rrd",
	      Output ++ stats_to_rrd(Stats, File)
      end,
      "",
      ?STATS_MODULES).

%%====================================================================
%% Internal functions
%%====================================================================

stats_to_text(Stats) ->
    lists:foldl(
      fun({Key,Val},Output) ->
	      case Key of
		  Key when is_integer(Key) ->
		      NewKey = integer_to_list(Key);
		  Key when is_binary(Key) ->
		      NewKey = binary_to_list(Key);
		  Key ->
		      NewKey = Key
	      end,
	      Output ++ NewKey ++ ":" ++ integer_to_list(Val) ++ "\n"
      end,
      "",
      Stats).
	      
stats_to_rrd(Stats, File) -> 
    Values = lists:map(
	       fun({_,V}) -> integer_to_list(V) end,
	       Stats
	      ),
    Command = ?RRDUPDATE ++ " " ++ File ++ " N:"++ string:join( Values, ":"),
    error_logger:info_report({?MODULE, ?LINE, {executing,Command}}),
    os:cmd(Command).
