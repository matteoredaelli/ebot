%%%-------------------------------------------------------------------
%%% File    : ebot_stats.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created : 16 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_stats).

-define(RRDUPDATE, "rrdupdate").
-define(STATS_MODULES, [ebot_amqp, ebot_db, ebot_memcache, ebot_web]).
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
	      Output ++ stats_to_text(Stats)
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
