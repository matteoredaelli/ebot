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
-behaviour(gen_server).

%% API
-export([
	 info/0,
	 start_link/0,
	 statistics/0,
	 fetch_url_get/1,
	 fetch_url_head/1,
	 fetch_url_links/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{config=[], good=0, bad=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout,?TIMEOUT}]).
info() ->
    gen_server:call(?MODULE, {info}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
fetch_url_get(URL) ->
    gen_server:call(?MODULE, {fetch_url_get, URL}).
fetch_url_head(URL) ->
    gen_server:call(?MODULE, {fetch_url_head, URL}).
fetch_url_links(URL) ->
    gen_server:call(?MODULE, {fetch_url_links, URL}).
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
	    State = #state{config=Config},
	    Options = get_config(request_options, State),
	    http:set_options(Options),
	    {ok, State};
	_Else ->
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
handle_call({fetch_url_get, URL}, _From, State) ->
    case Reply = fetch_url(URL, get, State) of
	{ok, {_Status, _Headers, _Body}} ->
	    NewState = State#state{good = State#state.good + 1};
	{error, Reason} ->
	    io:format("Error: ~s", [atom_to_list(Reason)]),
	    NewState = State#state{bad = State#state.bad + 1}
    end,
    {reply, Reply, NewState};
handle_call({fetch_url_head, URL}, _From, State) ->
    case Reply = fetch_url(URL, head, State) of
	{ok, {_Status, _Headers, _Body}} ->
	    NewState = State#state{good = State#state.good + 1};
	{error, Reason} ->
	    io:format("Error: ~s", [atom_to_list(Reason)]),
	    NewState = State#state{bad = State#state.bad + 1}
    end,
    {reply, Reply, NewState};
handle_call({fetch_url_links, URL}, _From, State) ->
    case fetch_url(URL, get, State) of
	{ok, {_Status, _Headers, Body}} ->
	    case Reply = ebot_html:get_links(Body, URL) of
		{error, _Reason} ->
		    NewState = State#state{bad = State#state.bad + 1};
		{ok, _Links} ->
		    NewState = State#state{good = State#state.good + 1}
	    end;
	{error, Reason} ->
	    io:format("Error: ~s", [atom_to_list(Reason)]),
	    NewState = State#state{bad = State#state.bad + 1},
	    Reply = {error, Reason}
    end,
    {reply, Reply, NewState};

handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = atom_to_list(?MODULE) ++ 
	": good=" ++ integer_to_list(State#state.good) ++
	", bad="  ++ integer_to_list(State#state.bad),
    NewState = State,
    {reply, Reply, NewState};

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

get_config(Option, State) ->
    proplists:get_value(Option, State#state.config).

fetch_url(URL, Command, State) ->
    Http_header = get_config(http_header, State),
    Request_options = get_config(request_options, State),
    Http_options = get_config(http_options, State),
    ebot_web_util:fetch_url(URL, Command, Http_header,Http_options,Request_options).
	    
