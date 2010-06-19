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
%%% File    : ebot_crawler.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_crawler).
-author("matteo.redaelli@@libero.it").
-define(SERVER, ?MODULE).

-behaviour(gen_server).

%% API
-export([
	 add_new_url/1,
	 add_visited_url/1,
	 info/0,
	 is_new_url/1,
	 is_visited_url/1,
	 show_new_urls/0,
	 show_visited_urls/0,
	 start_link/0,
	 start_workers/0,
	 stop_workers/0,
	 statistics/0,
	 workers_status/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  new_urls = queue:new(),
	  new_urls_counter = 0,
	  visited_urls = queue:new(),
	  visited_urls_counter = 0,
	  workers_status = started
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_new_url(Url) ->
    gen_server:cast(?MODULE, {add_new_url, Url}).
add_visited_url(Url) ->
    gen_server:cast(?MODULE, {add_visited_url, Url}).
is_new_url(Url) ->
    gen_server:call(?MODULE, {is_new_url, Url}).
is_visited_url(Url) ->
    gen_server:call(?MODULE, {is_visited_url, Url}).
info() ->
    gen_server:call(?MODULE, {info}).
show_new_urls() ->
    gen_server:call(?MODULE, {show_new_urls}).
show_visited_urls() ->
    gen_server:call(?MODULE, {show_visited_urls}).
start_workers() ->
    gen_server:call(?MODULE, {start_workers}).
stop_workers() ->
    gen_server:call(?MODULE, {stop_workers}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
workers_status() ->
    gen_server:call(?MODULE, {workers_status}).

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
    %% setup queues

    {ok, NewQueueSize} = ebot_util:get_env(cache_new_urls_queue_size),
    {ok, VisitedQueueSize} = ebot_util:get_env(cache_visited_urls_queue_size),

    %% filling queue with empty values: 
    %% in this way it is easier to check the max size of it:
    %% the queue is always full, everytime a new item is added,
    %% the oldest item is removed
    NewUrls = ebot_util:create_filled_queue(<<>>, NewQueueSize),
    VisitedUrls = ebot_util:create_filled_queue(<<>>, VisitedQueueSize),

    %% setup 
    case ebot_util:get_env(start_workers_at_boot) of
	{ok, true} ->
	    Crawlers_status = started;
	{ok, false} ->
	    Crawlers_status = stopped
    end,
    State =  #state{
      new_urls = NewUrls,
      visited_urls = VisitedUrls,
      workers_status = Crawlers_status
     },
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------;
handle_call({is_new_url, Url}, _From, State) ->
    Queue = State#state.new_urls,
    Result = queue:member(Url, Queue),
    {reply, Result, State};

handle_call({is_visited_url, Url}, _From, State) ->
    Queue = State#state.visited_urls,
    Result = queue:member(Url, Queue),
    {reply, Result, State};

handle_call({info}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({show_new_urls}, _From, State) ->
    Reply = show_queue(State#state.new_urls),
    {reply, Reply, State};

handle_call({show_visited_urls}, _From, State) ->
    Reply = show_queue(State#state.visited_urls),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    %% TODO: queue:len is slow O(n), erlang doc suggestes to keep track 
    %% of the size of queues.
    Reply = [
	     {<<"new_urls_counter">>, State#state.new_urls_counter},
	     {<<"visited_urls_counter">>, State#state.visited_urls_counter}
	    ],
    {reply, Reply, State};

handle_call({start_workers}, _From, State) ->
    error_logger:info_report({?MODULE, ?LINE, {start_workers, invoked}}),
    NewState = State#state{
		 workers_status = started
		},
    {reply, ok, NewState};

handle_call({stop_workers}, _From, State) ->
    error_logger:warning_report({?MODULE, ?LINE, {stop_workers, invoked}}),
    NewState = State#state{
		 workers_status = stopped
		},
    {reply, ok, NewState};

handle_call({workers_status}, _From, State) ->
    {reply, State#state.workers_status, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_new_url, Url}, State) ->
    Queue =  State#state.new_urls,
    case queue:member(Url, Queue) of
	true  ->
	    NewState = State;
	false ->
	    ebot_mq:send_url_new({Url,empty}),
	    {{value, _Item}, NewQueue} = queue:out(Queue),
	    NewState = State#state{
			 new_urls = queue:in(Url, NewQueue),
			 new_urls_counter = State#state.new_urls_counter + 1
			}
    end,
    {noreply, NewState};

handle_cast({add_visited_url, Url}, State) ->
    Queue =  State#state.visited_urls,
    case queue:member(Url, Queue) of
	true  ->
	    NewState = State;
	false ->
	    {{value, _Item}, NewQueue} = queue:out(Queue),
	    NewState = State#state{
			 visited_urls = queue:in(Url, NewQueue),
			 visited_urls_counter = State#state.visited_urls_counter + 1
			}
    end,
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

show_queue(Q) ->
    List = lists:map(
	     fun binary_to_list/1,
	     queue:to_list( Q)
	    ),
    string:join( List, ", ").
