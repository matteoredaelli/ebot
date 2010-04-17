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
	 add_todo_url/1,
	 add_visited_url/1,
	 crawl/0,
	 get_todo_url/0,
	 info/0,
	 is_visited_url/1,
	 show_todo_urls/0,
	 show_visited_urls/0,
	 start_crawlers/0,
	 stop_crawlers/0,
	 start_link/0,
	 statistics/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  config = [],
	  counter = 0,
	  crawlers = [],
	  status = started,
	  visited_urls = queue:new(),
	  todo_urls =  queue:new()
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

add_todo_url(Url) ->
    gen_server:cast(?MODULE, {add_todo_url, Url}).
add_visited_url(Url) ->
    gen_server:cast(?MODULE, {add_visited_url, Url}).
get_todo_url() ->
    gen_server:call(?MODULE, {get_todo_url}).
is_visited_url(Url) ->
    gen_server:call(?MODULE, {is_visited_url, Url}).
start_crawlers() ->
    gen_server:cast(?MODULE, {start_crawlers}).
stop_crawlers() ->
    gen_server:cast(?MODULE, {stop_crawlers}).
info() ->
    gen_server:call(?MODULE, {info}).
show_todo_urls() ->
    gen_server:call(?MODULE, {show_todo_urls}).
show_visited_urls() ->
    gen_server:call(?MODULE, {show_visited_urls}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).

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
	    {ok, #state{config=Config}};
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
%%--------------------------------------------------------------------;

handle_call({get_todo_url}, _From, State) ->
    Queue = State#state.todo_urls,
    case queue:out(Queue) of
	{{value, Result}, NewQueue} ->
	    NewState = State#state{ todo_urls = NewQueue };
	{empty, Queue} ->
	    NewState = State,
	    Result = empty
    end,
    {reply, Result, NewState};

handle_call({is_visited_url, Url}, _From, State) ->
    Visited = State#state.visited_urls,
    Result = queue:member(Url, Visited),
    {reply, Result, State};

handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
    {reply, Reply, State};

handle_call({show_todo_urls}, _From, State) ->
    Reply = show_queue(State#state.todo_urls),
    {reply, Reply, State};

handle_call({show_visited_urls}, _From, State) ->
    Reply = show_queue(State#state.visited_urls),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = atom_to_list(?MODULE) ++ 
	": counter=" ++ integer_to_list(State#state.counter),
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
handle_cast({add_todo_url, Url}, State) ->
    Queue =  State#state.todo_urls,
    case queue:member(Url, Queue) of
	true ->
	    NewState = State;
	false ->
	    NewState = State#state{todo_urls = queue:in(Url, Queue)}
    end,
    {noreply, NewState};

handle_cast({add_visited_url, Url}, State) ->
    Queue =  State#state.visited_urls,
    case queue:member(Url, Queue) of
	true  ->
	    NewState = State;
	false ->
	    NewState = State#state{
			 counter = State#state.counter + 1,
			 visited_urls = queue:in(Url, Queue)
			}
    end,
    {noreply, NewState};

handle_cast({start_crawlers}, State) ->
    Crawlers = lists:map(
		 fun(_C) -> spawn(?MODULE, crawl,[]) end,
		 lists:seq(1,5)
		 ),
    NewState = State#state{
		 crawlers = Crawlers,
		 status = started
		},
    {noreply, NewState};

handle_cast({stop_crawlers}, State) ->
    NewState = State#state{status = stopped},
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

% get_config(Option, State) ->
%    proplists:get_value(Option, State#state.config).

analyze_url_if_needed(Url) ->
    case is_visited_url(Url) of
	true ->
	    {ok, already_visited};
	false ->
	    analyze_url(Url)
    end.

analyze_url(Url) ->
    ebot_db:open_or_create_url(Url),
    ebot_db:update_url(Url),
    add_visited_url(Url).

analyze_url_links_if_html_page(Url) ->
    Doc = ebot_db:open_doc(Url),
    case ebot_db_util:is_html_doc(Doc) of
	true ->
	    analyze_url_links(Url);
	false ->
	    {ok, not_html_page}
    end.

analyze_url_links(Url) ->
    case ebot_web:fetch_url_links(Url) of
	{ok, Links} ->
	    %% removing already visited urls
	    NotVisitedLinks = lists:filter(
				fun(U) -> not is_visited_url(U) end,
				Links),
	    %% retrive Url from DB
	    lists:foreach(
	      fun(U) ->
		      %% creating the url in the database if it doen't exists
		      ebot_db:open_or_create_url(U),
		      case ebot_db:is_obsolete_url(U) of
			  {error, _Reason} ->
			      %% TODO
			      ko;
			  {ok, true} ->
			      % the url is obsolete
			      % it will be saved in the todo queue for processing
			      add_todo_url(U);
			  {ok, false} ->
			      % the url is not obsolete
			      % it will not be saved in the todo queue
			      % it will be saved in the visited queue
			      add_visited_url(U)
		      end
	      end,
	      NotVisitedLinks),
	    Result =  {ok, done};
	Error ->
	    Result = Error
    end,
   Result.

crawl() ->
    Url = get_todo_url(),
    crawl(Url).

crawl(empty) ->
    {ok, empty};
crawl(Url) ->
    analyze_url_if_needed(Url),
    analyze_url_links_if_html_page(Url).


show_queue(Q) ->
    List = lists:map(
	     fun binary_to_list/1,
	     queue:to_list( Q)
	    ),
    string:join( List, ", ").
