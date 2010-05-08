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

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([
	 crawl/2,
	 info/0,
	 show_active_crawlers/0,
	 start_crawlers/0,
	 start_link/0,
	 statistics/0,
	 stop_crawler/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{
	  config=[], 
	  status = started,
	  active_crawlers = [],
	  good=0, bad=0
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


info() ->
    gen_server:call(?MODULE, {info}).
show_active_crawlers() ->
    gen_server:call(?MODULE, {show_active_crawlers}).
start_crawlers() ->
    gen_server:cast(?MODULE, {start_crawlers}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).


stop_crawler(Crawler) ->
    gen_server:call(?MODULE, {stop_crawler, Crawler}).

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

handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Reply = atom_to_list(?MODULE) ++ 
	": good=" ++ integer_to_list(State#state.good) ++
	", bad="  ++ integer_to_list(State#state.bad),
    NewState = State,
    {reply, Reply, NewState};

handle_call({show_active_crawlers}, _From, State) ->
    Reply = State#state.active_crawlers,
    {reply, Reply, State};

handle_call({stop_crawler, {Depth,Pid}}, _From, State) ->
    Reply = ok,
    NewState = State#state{
		 active_crawlers = lists:delete({Depth,Pid}, 
						State#state.active_crawlers)
		},
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

handle_cast({start_crawlers}, State) ->
    Pools = get_config(crawler_pools, State),
    NewCrawlers = lists:foldl(
      fun({Depth,Tot}, Crawlers) ->
	      OtherCrawlers = start_crawlers(Depth, Tot, State),
	      lists:append( Crawlers, OtherCrawlers)
      end,
      State#state.active_crawlers,
      Pools), 
    NewState = State#state{
		 active_crawlers = NewCrawlers
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

analyze_url(empty, _State) ->
    {ok, empty};
analyze_url(Url, State) ->
    analyze_url_from_url_status(Url, ebot_db:url_status(Url), State).

analyze_url_header(Url, State) ->
    case Result = fetch_url(Url, head, State) of
	{error, Reason} -> 
	    {error, Reason};
	Result ->
	    Options = [{head, Result}],
	    ebot_db:update_url(Url, Options),
	    ebot_memcache:add_visited_url(Url)
    end.

analyze_url_body(Url, State) ->
    case fetch_url_links(Url, State) of
	{ok, Links} ->
	    LinksCount = length(Links),
	    %% normalizing Links
	    NormalizeOptions = get_config(normalize_url, State),
	    NormalizedLinks = lists:map(
				fun(U) -> 
					ebot_url_util:normalize_url(U, NormalizeOptions)
				end,
				Links),
	    
	    %% removing duplicates
	    UniqueLinks = ebot_util:remove_duplicates(NormalizedLinks),

	    %% TODO
	    %% removing unwanted urls
	    %%

	    %% removing already visited urls and not valid
	    NotVisitedLinks = lists:filter(
				fun(U) -> 
					(not ebot_memcache:is_visited_url(U)) andalso
					    is_valid_url(U, State)
				end,
				UniqueLinks),

	    %% retrive Url from DB
	    lists:foreach(
	      fun(U) ->
		      %% creating the url in the database if it doen't exists
		      ebot_db:open_or_create_url(U),
		      ebot_memcache:add_new_url(U),
		      Options = [{referral, Url}],
		      ebot_db:update_url(U, Options)
	      end,
	      NotVisitedLinks),
	    %% UPDATE ebot-body-visited
	    Options = [body_timestamp, {link_counts,  LinksCount}],
	    ebot_db:update_url(Url, Options),
	    Result =  ok;
	Error ->
	    Result = Error
    end,
   Result.

analyze_url_from_url_status(Url, not_found, State) ->
    ebot_db:open_or_create_url(Url),
    analyze_url_header(Url, State),
    %% not suse if teh url is html or not and so coming back 
    %% to check url_status
    analyze_url(Url, State);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,new}}, State) ->
    analyze_url_body(Url, State);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,obsolete}}, State) ->
    analyze_url_body(Url, State);
analyze_url_from_url_status(_Url, {ok, {header, updated}, {body, _Status}}, _State) ->
    %% skipped od updated
    ok;
analyze_url_from_url_status(Url, {ok, {header, _HeaderStatus}, {body,_}}, State) ->
    analyze_url_header(Url, State),
    analyze_url(Url, State).

  
crawl(Depth, State) ->
    Url = ebot_amqp:get_new_url(Depth),
    analyze_url(Url, State),
    crawl(Depth, State).

get_config(Option, State) ->
    proplists:get_value(Option, State#state.config).

fetch_url(URL, Command, State) ->
    Http_header = get_config(http_header, State),
    Request_options = get_config(request_options, State),
    Http_options = get_config(http_options, State),
    ebot_web_util:fetch_url(URL, Command, Http_header,Http_options,Request_options).

fetch_url_links(URL, State) ->
    case fetch_url(URL, get, State) of
	{ok, {_Status, _Headers, Body}} ->
	    {ok, ebot_html_util:get_links(Body, URL)};
	{error, Reason} ->
	    io:format("Error: ~s", [atom_to_list(Reason)]),
	    {error, Reason}
    end.

is_valid_url(Url, State) when is_binary(Url) ->
    is_valid_url(binary_to_list(Url), State);

is_valid_url(Url, State) ->
    MimeAnyRE = get_config(mime_any_regexps, State),
    UrlAllRE = get_config(url_all_regexps, State),
    UrlAnyRE = get_config(url_any_regexps, State),
    ebot_util:is_valid_using_all_regexps(Url, UrlAllRE) andalso
	ebot_util:is_valid_using_any_regexps(Url, UrlAnyRE) andalso
	ebot_url_util:is_valid_url_using_any_mime_regexps(Url, MimeAnyRE).
	    
start_crawlers(Depth, Total, State) -> 
    lists:map(
      fun(_) ->
	      {Depth, spawn( ?MODULE, crawl, [Depth, State])} end,
      lists:seq(1,Total)).
