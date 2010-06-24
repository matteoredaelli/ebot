% EBOT, an erlang web crawler.
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
%%% File    : ebot_worker_util.erl
%%% Author  : matteo <matteo.redaelli@libero.it>
%%% Description : 
%%%
%%% Created : 13 Jun 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_worker_util).

-define(WORKER_START_FUNCTION, run).

%% API
-export([
	 check_recover_workers/1,
	 create_workers/1,
	 remove_worker/2,
	 start_worker/2,
	 start_workers/3,
	 start_workers/2,
	 statistics/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

check_recover_workers({Type, Workers}) ->
    NewWorkers = lists:map( 
		   fun({Depth, Pid}) ->
			   case erlang:is_process_alive(Pid) of
			       true ->
				   error_logger:info_report({?MODULE, ?LINE, 
							     {check_recover_worker, status,
							      proplists:get_value(status, process_info(Pid)) }}),
				   {Depth, Pid};
			       false ->
				   error_logger:warning_report({?MODULE, ?LINE, 
								{check_recover_workers, recovering_dead_worker}}),
				   NewPid = spawn_worker(Depth, Type),
				   {Depth, NewPid}
			   end
		   end,
		   Workers),
    {Type, NewWorkers}.

create_workers(Type) ->
    {Type, []}.

remove_worker({Depth, Pid}, {Type,Workers}) ->
    {Type, lists:delete({Depth, Pid}, Workers)}.

start_workers(Depth, Total, {Type,Workers}) -> 
    lists:foldl(
      fun(_, {T,W}) -> start_worker(Depth, {T,W}) end,
      {Type, Workers},
      lists:seq(1,Total)
     ).

start_workers(Pool, {Type,Workers}) -> 
    lists:foldl(
      fun({Depth, Total}, {T,W}) -> start_workers(Depth, Total, {T,W}) end,
      {Type,Workers},
      Pool
     ).

statistics({Type,Workers}) ->
    {ok, TotQueues} = ebot_util:get_env(mq_priority_url_queues),
    lists:map(
      fun(Depth) ->
	      {Type, NewWorkers} = filter_workers_by_depth(Depth, {Type, Workers}),
	      {"worker-" ++ atom_to_list(Type) ++ "-" ++ integer_to_list(Depth), length(NewWorkers)}
      end,
      lists:seq(0, TotQueues)
     ).

%%====================================================================
%% Internal functions
%%====================================================================

add_worker({Depth, Pid}, {Type,Workers}) ->
    {Type, [{Depth, Pid}|Workers]}.
    
filter_workers_by_depth(Depth, {Type, Workers}) ->
    NewWorkers = lists:filter(
		   fun({D,_}) -> D == Depth end,
		   Workers),
    {Type, NewWorkers}.

spawn_worker(Depth, Type) ->
    spawn(worker_module(Type), ?WORKER_START_FUNCTION, [Depth]).

start_worker(Depth, {Type, Workers}) ->
    Pid = spawn_worker(Depth, Type),
    add_worker({Depth, Pid}, {Type,Workers}).

worker_module(Type) ->
    list_to_atom("ebot_" ++ atom_to_list(Type)).

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_worker_test() ->
    {web, Workers} = create_workers(web),

    ?assertEqual( [], Workers),
    {web, Workers2} = add_worker({0,pid1}, {web,Workers}),
    ?assertEqual( [{0,pid1}], Workers2 ),
    ?assertEqual( {web,[{0,pid1}]}, filter_workers_by_depth(0, {web,Workers2})),
    ?assertEqual( {web,[]}, filter_workers_by_depth(1, {web,Workers2})),
    ?assertEqual(ebot_web, worker_module(web)).  
-endif.
