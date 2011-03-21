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
%%% File    : ebot_db.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_db).
-author("matteo.redaelli@libero.it").
-define(SERVER, ?MODULE).

-behaviour(gen_server).

%% example of a http header
%%[{"connection","Keep-Alive"},
%% {"date","Tue, 30 Mar 2010 16:42:18 GMT"},
%% {"accept-ranges","bytes"},
%% {"etag","\"4280393-2c2b-47c53ea7c7f40\""},
%% {"server","Apache/2.2"},
%% {"content-length","11307"},
%% {"content-type","text/html"},
%% {"last-modified","Mon, 04 Jan 2010 10:09:25 GMT"},
%% {"keep-alive","timeout=15, max=100"}]

%%     [{"connection","Keep-Alive"},
%%      {"date","Wed, 31 Mar 2010 14:10:41 GMT"},
%%      {"server","Apache/2.2"},
%%      {"content-type","text/html; charset=UTF-8"},
%%      {"x-powered-by","PHP/5.3.2"},
%%      {"x-pingback",
%%       "http://www.redaelli.org/matteo/blog/xmlrpc.php"},
%%      {"link","<http://wp.me/Lb2b>; rel=shortlink"},
%%      {"keep-alive","timeout=15, max=99"}],


%% API
-export([
	 list_docs/0,
	 start_link/0,
	 statistics/0,
	 create_url/1,
	 delete_doc/1,
	 delete_all_docs/0,
	 open_doc/1,
	 open_or_create_crawler/1,
	 open_or_create_doc/2,
	 open_or_create_url/1,
	 update_doc/2,
	 url_status/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record( state,
	 {
	   db,
	   good=0, 
	   bad=0
	  }
	).

-include("ebot.hrl").
-include("deps/couchbeam/include/couchbeam.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
create_url(ID) ->
    gen_server:call(?MODULE, {create_doc, ID, url}).
delete_doc(ID) ->
    gen_server:call(?MODULE, {delete_doc, ID}).
delete_all_docs() ->
    gen_server:cast(?MODULE, {delete_all_docs}).
list_docs() ->
    gen_server:call(?MODULE, {list_docs}).
open_doc(ID) ->
    gen_server:call(?MODULE, {open_doc, ID}).
open_or_create_crawler(ID) ->
    gen_server:call(?MODULE, {open_or_create_doc, ID, crawler}).
open_or_create_doc(ID, DocType) ->
    gen_server:call(?MODULE, {open_or_create_doc, ID, DocType}).
open_or_create_url(ID) ->
    gen_server:call(?MODULE, {open_or_create_doc, ID, url}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
update_doc(ID, Options) ->
    gen_server:call(?MODULE, {update_doc, ID, Options}).
url_status(ID, Days) ->
    gen_server:call(?MODULE, {url_status, ID, Days}).
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
    case ebot_db_util:open_or_create_db() of
	{ok, DB} ->
	    State = #state{db=DB},
	    {ok, State};
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {init, cannot_open_db, ?EBOT_DB_BACKEND, Else}}),
	    Else
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create_doc, ID, DocType}, _From, State) ->
    Reply = ebot_db_util:create_doc(State#state.db, ID, DocType),
    {reply, Reply, State};
handle_call({delete_doc, ID}, _From, State) ->
    Reply = ebot_db_util:delete_doc(State#state.db, ID),
    {reply, Reply, State};
handle_call({list_docs}, _From, State) ->
    Reply =  ebot_db_util:list_docs(State#state.db),
    {reply, Reply, State};
handle_call({open_doc, ID}, _From, State) ->
    Reply = ebot_db_util:open_doc(State#state.db, ID),
    {reply, Reply, State};
handle_call({open_or_create_doc, ID, DocType}, _From, State) ->
    Reply = ebot_db_util:open_or_create_doc(State#state.db, ID, DocType),
    {reply, Reply, State};		 
handle_call({url_status, ID, Days}, _From, State) ->
    Reply = ebot_db_doc_url:url_status(State#state.db, ID, Days),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = ?EBOT_DB_BACKEND:statistics(State#state.db),
    {reply, Reply, State};

handle_call({update_doc, ID, Options}, _From, State) ->
    Reply = ebot_db_util:update_doc(State#state.db, ID, Options),
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
handle_cast({delete_all_docs}, State) ->
    ?EBOT_DB_BACKEND:delete_all_docs(State#state.db),
    {noreply, State};
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

%%====================================================================
%% EUNIT TESTS
%%====================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ebot_db_test() ->
    ?assertEqual(true, true).
-endif.
