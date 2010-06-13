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
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created : 13 Jun 2010 by matteo <matteo.redaelli@libero,it>
%%%-------------------------------------------------------------------
-module(ebot_crawler).
-author("matteo.redaelli@@libero.it").

-define(SERVER, ?MODULE).

-behaviour(gen_server).


%% API
-export([start_link/0,
	 crawlers_status/0,
	 start_crawlers/0,
	 stop_crawlers/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  crawlers_status = started
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

crawlers_status() ->
    gen_server:call(?MODULE, {crawlers_status}).
start_crawlers() ->
    gen_server:call(?MODULE, {start_crawlers}).
stop_crawlers() ->
    gen_server:call(?MODULE, {stop_crawlers}).

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
    case ebot_util:get_env(start_crawlers_at_boot) of
	{ok, true} ->
	    Crawlers_status = started;
	{ok, false} ->
	    Crawlers_status = stopped
    end,
    State =  #state{
      crawlers_status = Crawlers_status
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
%%--------------------------------------------------------------------
handle_call({crawlers_status}, _From, State) ->
    {reply, State#state.crawlers_status, State};
handle_call({start_crawlers}, _From, State) ->
    error_logger:info_report({?MODULE, ?LINE, {start_crawlers, invoked}}),
    NewState = State#state{
		 crawlers_status = started
		},
    ebot_web:start_crawlers(),
    {reply, ok, NewState};

handle_call({stop_crawlers}, _From, State) ->
    error_logger:warning_report({?MODULE, ?LINE, {stop_crawlers, invoked}}),
    NewState = State#state{
		 crawlers_status = stopped
		},
    {reply, ok, NewState};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
