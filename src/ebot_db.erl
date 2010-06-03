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
	 info/0,
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
	   config=[], 
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
info() ->
    gen_server:call(?MODULE, {info}).
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
    case ebot_util:load_settings(?MODULE) of
	{ok, Config} ->
	    Hostname = proplists:get_value(hostname, Config),
	    Port = proplists:get_value(port, Config),
	    case ?EBOT_DB_BACKEND of
		ebot_db_backend_couchdb ->
		    couchbeam:start(),
		    couchbeam_server:start_connection_link(
		      #couchdb_params{host=Hostname, port=Port} 
		     ),
		    Ebotdb = couchbeam_server:open_or_create_db(default, "ebot"),
		    State = #state{config=Config, db=Ebotdb},
		    {ok, State};
		ebot_db_backend_riak_pb ->
		    {ok, Pid} = riakc_pb_socket:start_link(Hostname, Port),
		    State = #state{config=Config, db=Pid},
		    {ok, State};
		else ->
		    error_logger:error_report({?MODULE, ?LINE, {init, unsupported_backend, ?EBOT_DB_BACKEND}}),
		    {error, unsupported_backend}
	    end;
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {cannot_load_configuration_file, Else}}),
	    {error, cannot_load_configuration}
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
handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
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

%get_config(Option, State) ->
%    proplists:get_value(Option, State#state.config).



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
    ebot_db:update_url(Url, [{update_field_key_value, Key, 0}]),
    Doc3 = ebot_db:open_url(Url),
    ?assertEqual({ok,0}, dict:find(Key, Doc3)),
    ebot_db:delete_url(Url).
%    ?assertEqual(not_found,  ebot_db:open_url(Url)).
-endif.
