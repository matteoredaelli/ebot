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
%%% File    : ebot_web.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_web).
-author("matteo.redaelli@libero.it").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(WORKER_TYPE, web).

-include("ebot.hrl").

-behaviour(gen_server).

%% API
-export([
	 check_recover_workers/0,
	 run/1,
	 info/0,
	 remove_worker/1,
	 get_workers/0,
	 start_workers/0,
	 start_workers/2,
	 start_link/0,
	 statistics/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{
	  workers = ebot_worker_util:create_workers(?WORKER_TYPE)
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout,?TIMEOUT}]).
check_recover_workers() ->
    gen_server:call(?MODULE, {check_recover_workers}).
info() ->
    gen_server:call(?MODULE, {info}).
get_workers() ->
    gen_server:call(?MODULE, {get_workers}).
start_workers() ->
    gen_server:cast(?MODULE, {start_workers}).
start_workers(Depth, Tot) ->
    gen_server:cast(?MODULE, {start_workers, Depth, Tot}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
remove_worker(Worker) ->
    gen_server:cast(?MODULE, {remove_worker, Worker}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, Options} = ebot_util:get_env(web_request_options),
    httpc:set_options(Options),
    case ebot_util:get_env(start_workers_at_boot) of
	{ok, true} ->
	    State = start_workers(#state{});
	{ok, false} ->
	    State = #state{}
    end,
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_recover_workers}, _From, State) ->
    Workers = ebot_worker_util:check_recover_workers(State#state.workers),
    NewState = State#state{
		 workers = Workers
		},
    {reply, Workers, NewState};

handle_call({info}, _From, State) ->
    {reply, ok, State};

handle_call({statistics}, _From, State) ->
    Reply = ebot_worker_util:statistics(State#state.workers),
    {reply, Reply, State};

handle_call({get_workers}, _From, State) ->
    {?WORKER_TYPE, Reply} = State#state.workers,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({remove_worker, Worker}, State) ->
    NewState = State#state{
		 workers = ebot_worker_util:remove_worker(Worker, 
							  State#state.workers)
		},
    {noreply, NewState};

handle_cast({start_workers}, State) ->
    NewState = start_workers(State),
    {noreply, NewState};

handle_cast({start_workers, Depth, Tot}, State) ->
    NewState = State#state{
		 workers = ebot_worker_util:start_workers(Depth,Tot, 
							  State#state.workers)
		},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run(Depth) ->
    case ebot_mq:receive_url_new(Depth) of
	{ok, {Url, _}} ->
	    ebot_crawler:add_visited_url(Url),
	    Result = ebot_web_util:fetch_url_with_only_html_body(Url),
	    ebot_mq:send_url_fetched({Url,Result});
	{error, _Reason} ->
	    error_logger:info_report({?MODULE, ?LINE, {run, no_queued_urls, waiting}}),
	    timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT )
    end,
    case ebot_crawler:workers_status() of
	started ->
	    {ok, Sleep} = ebot_util:get_env(workers_sleep_time),
	    timer:sleep( Sleep ),
	    run(Depth);
	stopped ->
	    error_logger:warning_report({?MODULE, ?LINE, {stopping_worker, self()}}),
	    remove_worker( {Depth, self()} )
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_workers(State) ->
    {ok, Pool} = ebot_util:get_env(workers_pool),
    State#state{
      workers = ebot_worker_util:start_workers(Pool, 
					       State#state.workers)
     }.

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_web_test() ->
    Url =  <<"http://www.redaelli.org/matteo/ebot_test/">>,

    ExpectedUrlLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>,
			     <<"http://www.redaelli.org/">>,
			     <<"http://www.redaelli.org/matteo">>,
			     <<"http://www.redaelli.org/matteo/ebot_test/dir1">>
		       ],
    UrlLinks = ebot_web_util:fetch_url_links(Url),
    ?assertEqual( {ok, ExpectedUrlLinks}, UrlLinks),
    ExpectedExternalLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>],
    ?assertEqual(ExpectedExternalLinks,  
		 ebot_url_util:filter_external_links(Url, ExpectedUrlLinks)).
-endif.
