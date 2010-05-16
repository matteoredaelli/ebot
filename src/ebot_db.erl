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
	 info_db/0,
	 start_link/0,
	 statistics/0,
	 create_url/1,
	 create_view/1,
	 open_doc/1,
	 open_or_create_url/1,
	 query_view/2,
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

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
info() ->
    gen_server:call(?MODULE, {info}).
info_db() ->
    gen_server:call(?MODULE, {info_db}).
url_status(Url, Days) ->
    gen_server:call(?MODULE, {url_status, Url, Days}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
open_doc(ID) ->
    gen_server:call(?MODULE, {open_doc, ID}).
open_or_create_url(Url) ->
    gen_server:call(?MODULE, {open_or_create_url, Url}).
query_view({DocName, View}, Options) ->
    gen_server:call(?MODULE, {query_view, DocName, View, Options}).
create_url(Url) ->
    gen_server:cast(?MODULE, {create_url, Url}).
create_view(Doc) ->
    gen_server:cast(?MODULE, {create_view, Doc}).
update_url(Url, Options) ->
    gen_server:call(?MODULE, {update_url, Url, Options}).
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
	    couchbeam_server:start_connection_link(),
	    Ebotdb = couchbeam_server:open_db(default, "ebot"),
	    State = #state{config=Config, db=Ebotdb},
	    {ok, State};
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
handle_call({open_doc, ID}, _From, State) ->
    Reply = ebot_db_util:open_doc(State#state.db, ID),
    {reply, Reply, State};
handle_call({open_or_create_url, Url}, _From, State) ->
    Reply = ebot_db_util:open_or_create_url(State#state.db, Url),
    {reply, Reply, State};		 
handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
    {reply, Reply, State};
handle_call({info_db}, _From, State) ->
    Reply = couchbeam_db:info(State#state.db),
    {reply, Reply, State};
handle_call({query_view, {DocName, View}, Options}, _From, State) ->
    Reply = couchbeam_db:query_view(State#state.db, {DocName, View}, Options),
    {reply, Reply, State};
handle_call({url_status, Url, Days}, _From, State) ->
    Reply = ebot_db_util:url_status(State#state.db, Url, Days),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Doc = couchbeam_db:info(State#state.db),
    DiskSize = round(proplists:get_value(<<"disk_size">>, Doc) / 1024 / 1024),
    DocCount = proplists:get_value(<<"doc_count">>, Doc),
    Reply = [{<<"disk_size">>, DiskSize}, {<<"doc_count">>,DocCount}],
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
handle_cast({create_url, Url}, State) ->
    ebot_db_util:create_url(State#state.db, Url),
    {noreply, State};
handle_cast({create_view, Doc}, State) ->
    couchbeam_db:save_doc(State#state.db, Doc),
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


