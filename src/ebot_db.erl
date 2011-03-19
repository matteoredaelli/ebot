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
	 list_urls/0,
	 start_link/0,
	 statistics/0,
	 create_url/1,
	 delete_url/1,
	 empty_db_urls/0,
	 open_url/1,
	 open_or_create_url/1,
	 update_url/2,
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

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
create_url(Url) ->
    gen_server:call(?MODULE, {create_url, Url}).
delete_url(Url) ->
    gen_server:call(?MODULE, {delete_url, Url}).
empty_db_urls() ->
    gen_server:cast(?MODULE, {empty_db_urls}).
list_urls() ->
    gen_server:call(?MODULE, {list_urls}).
open_url(ID) ->
    gen_server:call(?MODULE, {open_url, ID}).
open_or_create_url(Url) ->
    gen_server:call(?MODULE, {open_or_create_url, Url}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
update_url(Url, Options) ->
    gen_server:call(?MODULE, {update_url, Url, Options}).
url_status(Url, Days) ->
    gen_server:call(?MODULE, {url_status, Url, Days}).
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
    {ok, Hostname} = ebot_util:get_env(db_hostname),
    {ok, Port} = ebot_util:get_env(db_port),
    case ?EBOT_DB_BACKEND of
	ebot_db_backend_couchdb ->
	    application:start(ibrowse),
	    application:start(couchbeam),
	    Prefix = "",
	    Options = [],
	    Conn = couchbeam:server_connection(Hostname, Port, Prefix, Options),
	    case couchbeam:server_info(Conn) of
		{ok, _Version} ->
		    {ok, Ebotdb} = couchbeam:open_or_create_db(Conn, "ebot", []),
		    State = #state{db=Ebotdb},
		    {ok, State};
		Else ->
		    error_logger:error_report({?MODULE, ?LINE, {init, cannot_connect_to_db, ?EBOT_DB_BACKEND, Else}}),
		    Else
	    end;
	ebot_db_backend_riak_pb ->
	    case riakc_pb_socket:start_link(Hostname, Port) of
		{ok, Pid} ->
		    State = #state{db=Pid},
		    {ok, State};
		Else ->
		    error_logger:error_report({?MODULE, ?LINE, {init, cannot_connect_to_db, ?EBOT_DB_BACKEND, Else}}),
		    Else
	    end;
	_Else ->
	    error_logger:error_report({?MODULE, ?LINE, {init, unsupported_backend, ?EBOT_DB_BACKEND}}),
	    {error, unsupported_backend}
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
handle_call({create_url, Url}, _From, State) ->
    Reply = ebot_db_util:create_url(State#state.db, Url),
    {reply, Reply, State};
handle_call({delete_url, Url}, _From, State) ->
    Reply =  ?EBOT_DB_BACKEND:delete_url(State#state.db, Url),
    {reply, Reply, State};
handle_call({list_urls}, _From, State) ->
    Reply =  ?EBOT_DB_BACKEND:list_urls(State#state.db),
    {reply, Reply, State};
handle_call({open_url, ID}, _From, State) ->
    Reply = ebot_db_util:open_url(State#state.db, ID),
    {reply, Reply, State};
handle_call({open_or_create_url, Url}, _From, State) ->
    Reply = ebot_db_util:open_or_create_url(State#state.db, Url),
    {reply, Reply, State};		 
handle_call({url_status, Url, Days}, _From, State) ->
    Reply = ebot_db_util:url_status(State#state.db, Url, Days),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = ?EBOT_DB_BACKEND:statistics(State#state.db),
    {reply, Reply, State};

handle_call({update_url, Url, Options}, _From, State) ->
    Reply = ebot_db_util:update_url(State#state.db, Url, Options),
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
handle_cast({empty_db_urls}, State) ->
    ?EBOT_DB_BACKEND:empty_db_urls(State#state.db),
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

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_db_test() ->
    Url = <<"http://www.redaelli.org/matteo/ebot_test/">>,
    Key = <<"ebot_referrals_count">>,
    ebot_db:open_or_create_url(Url),
    Doc = ebot_db:open_url(Url),
    ?assertEqual({ok,0}, dict:find(Key, Doc)),
    ebot_db:update_url(Url, [{update_field_key_value, Key, 1}]),
    Doc2 = ebot_db:open_url(Url),
    ?assertEqual({ok,1}, dict:find(Key, Doc2)),
    ebot_db:update_url(Url, [
			     {update_field_key_value, <<"ebot_head_error">>, <<"">> },
			     {update_field_key_value, <<"content_length">>, <<"12345">> },
			     {update_field_timestamp,<<"ebot_head_visited">>},
			     {update_field_counter,<<"ebot_visits_count">>},
			     {update_field_key_value,<<"ebot_errors_count">>,1},
			     {update_field_key_value, <<"content-type">>, <<"text/html; charset=UTF-8">>}
			    ]),
    Doc3 = ebot_db:open_url(Url),
    ?assertEqual({ok,<<"">>}, dict:find(<<"ebot_head_error">>, Doc3)),
    ?assertEqual({ok,<<"12345">>}, dict:find(<<"content_length">>, Doc3)),
    ?assertEqual({ok,1}, dict:find(<<"ebot_errors_count">>, Doc3)),
    ebot_db:update_url(Url, [
			     {head,
			      {{"HTTP/1.1",200,"OK"},
			       [{"cache-control",
				 "private, max-age=0, must-revalidate"},
				{"connection","keep-alive"},
				{"date","Wed, 22 Sep 2010 19:12:39 GMT"},
				{"via","1.1 varnish"},
				{"age","0"},
				{"etag",
				 "\"9de4ba5a2cf11a66a3543fd2c07ae683\""},
				{"server","Apache"},
				{"vary","Accept-Encoding"},
				{"content-length","11521"},
				{"content-type","text/html; charset=utf-8"},
				{"x-powered-by",
                                   "Phusion Passenger (mod_rails/mod_rack) 2.2.11"},
				{"x-runtime","20"},
				{"set-cookie",
				 "_gitorious_sess=8b512c1b5212a4b330ee2e8a1673153e; domain=.gitorious.org; path=/; expires=Wed, 13 Oct 2010 19:12:39 GMT; HttpOnly"},
				{"status","200"},
				{"x-varnish","942297164"}
			       ],
			       empty
			      },
			      [<<"content-length">>,<<"content-type">>,
			       <<"server">>,<<"x-powered-by">>, <<"UNKNOWNHEADER">>
			      ]
			     }
			    ]
		      ),
    
    
%   What happens if you update a not existing url?
    ebot_db:update_url(<<"http://code.google.com/">>,
		       [
			{update_field_counter,<<"ebot_errors_count">>},
			{update_field_key_value,<<"ebot_head_error">>, <<"timeout">>}
		       ]
		      ),
    Doc4 = ebot_db:open_url(<<"http://code.google.com/">>),
    ?assertEqual({ok,<<"timeout">>}, dict:find(<<"ebot_head_error">>, Doc4)).
    
%    ebot_db:delete_url(Url).
%    ?assertEqual(not_found,  ebot_db:open_url(Url)).
-endif.
