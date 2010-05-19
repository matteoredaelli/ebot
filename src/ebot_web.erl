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
	 check_recover_crawlers/0,
	 crawl/2,
	 crawlers_status/0,
	 fetch_url/2,
	 fetch_url_links/1,
	 info/0,
	 show_crawlers_list/0,
	 start_crawlers/0,
	 start_link/0,
	 statistics/0,
	 stop_crawler/1,
	 stop_crawlers/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{
	  config=[], 
	  crawlers_status = started,  %% or stopped
	  crawlers_list = [],
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
check_recover_crawlers() ->
    gen_server:call(?MODULE, {check_recover_crawlers}).
crawlers_status() ->
    gen_server:call(?MODULE, {crawlers_status}).
fetch_url(Url, Command) ->
    gen_server:call(?MODULE, {fetch_url, Url, Command}).
fetch_url_links(Url) ->
    gen_server:call(?MODULE, {fetch_url_links, Url}).
info() ->
    gen_server:call(?MODULE, {info}).
show_crawlers_list() ->
    gen_server:call(?MODULE, {show_crawlers_list}).
start_crawlers() ->
    gen_server:cast(?MODULE, {start_crawlers}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
stop_crawler(Crawler) ->
    gen_server:call(?MODULE, {stop_crawler, Crawler}).
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
    case ebot_util:load_settings(?MODULE) of
	{ok, Config} ->
	    State = #state{config=Config},
	    Options = get_config(request_options, State),
	    http:set_options(Options),
	    case get_config(start_crawlers_at_boot, State) of
		true ->
		    Crawlers_status = started,
		    NewState = start_crawlers(State);
		false ->
		    Crawlers_status = stopped,
		    NewState = State
	    end,
	    {ok, NewState#state{crawlers_status = Crawlers_status}};
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
handle_call({check_recover_crawlers}, _From, State) ->
    NewCrawlers = check_recover_crawlers(State),
    NewState = State#state{
		 crawlers_list = NewCrawlers
		},
    {reply, NewCrawlers, NewState};
handle_call({crawlers_status}, _From, State) ->
    {reply, State#state.crawlers_status, State};
handle_call({fetch_url, Url, Command}, _From, State) ->
    Reply = fetch_url(Url, Command, State),
    {reply, Reply, State};
handle_call({fetch_url_links, Url}, _From, State) ->
    Reply = fetch_url_links(Url, State),
    {reply, Reply, State};
handle_call({info}, _From, State) ->
    Reply = ebot_util:info(State#state.config),
    {reply, Reply, State};

handle_call({statistics}, _From, State) ->
    Pools = get_config(crawler_pools, State),
    Crawlers = State#state.crawlers_list,

    Depths = lists:sort(
	     lists:map( 
	       fun({D,_}) -> D end,
	       Pools
	      )), 
    Reply = lists:map(
	      fun(Depth) ->
		      Pids = lists:filter(
			       fun({D,_}) ->  D == Depth end,
			       Crawlers),
		      {Depth,length(Pids)}
	      end,
	      Depths),
    {reply, Reply, State};

handle_call({show_crawlers_list}, _From, State) ->
    Reply = State#state.crawlers_list,
    {reply, Reply, State};

handle_call({stop_crawler, {Depth,Pid}}, _From, State) ->
    Reply = ok,
    NewState = State#state{
		 crawlers_list = lists:delete({Depth,Pid}, 
						State#state.crawlers_list)
		},
    {reply, Reply, NewState};

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

handle_cast({start_crawlers}, State) ->
    NewState = start_crawlers(State),
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
    Days = get_config(obsolete_urls_after_days, State),
    analyze_url_from_url_status(Url, ebot_db:url_status(Url, Days), State).

analyze_url_header(Url, State) ->
    ebot_memcache:add_visited_url(Url),
    case Result = fetch_url(Url, head, State) of
	{error, Reason} -> 
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_header, Url, skipping_url, Reason}}),
	    Options = [errors_count],
	    %% TODO: instead of only Url, it would be nice to send {Url, Reason}
	    %% like <<"https://github.com/login/,https_through_proxy_is_not_currently_supported">>
	    ebot_amqp:add_refused_url(Url),
	    {error, Reason};
	Result ->
	    Options = [{head, Result}, head_timestamp, visits_count, reset_errors_count]
    end,
    ebot_db:update_url(Url, Options),
    Result.

analyze_url_body(Url, State) ->
    error_logger:info_report({?MODULE, ?LINE, {getting_links_of, Url}}),
    case fetch_url_links(Url, State) of
	{ok, Links} ->
	    LinksCount = length(Links),
	    %% normalizing Links
	    error_logger:info_report({?MODULE, ?LINE, {normalizing_links_of, Url}}),
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
		      error_logger:info_report({?MODULE, ?LINE, {adding, U, from, Url}}),
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
    analyze_url(Url, State);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,new}}, State) ->
    analyze_url_body(Url, State);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,obsolete}}, State) ->
    analyze_url_body(Url, State);
analyze_url_from_url_status(_Url, {ok, {header, updated}, {body, _Status}}, _State) ->
    %% skipped od updated
    ok;
analyze_url_from_url_status(Url, {ok, {header, _HeaderStatus}, {body,_}}, State) ->
    case analyze_url_header(Url, State) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    %% not sure if the url is html or not and so coming back 
	    %% to check url_status
	    analyze_url(Url, State);
	Other ->
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_from_url_status, Url, error, Other}}),
	    Other
    end.

check_recover_crawlers(State) ->
    Crawlers = State#state.crawlers_list,
    list:map( 
      fun({Depth, Pid}) ->
	      case is_process_alive(Pid) of
		  true ->
		      {Depth, Pid};
		  false ->
		      error_logger:warning_report({?MODULE, ?LINE, {check_recover_crawlers, recovering_dead_crawler}}),
		      start_crawler(Depth, State)
	      end
      end,
      Crawlers).

crawl(Depth, State) ->
    Url = ebot_amqp:get_new_url(Depth),
    analyze_url(Url, State),
    case ebot_web:crawlers_status() of
	started ->
	    timer:sleep( get_config(crawlers_sleep_time, State) ),
	    crawl(Depth, State);
	stopped ->
	    error_logger:warning_report({?MODULE, ?LINE, {stopping_crawler, self()}}),
	    stop_crawler( {Depth, self()} )
    end.

get_config(Option, State) ->
    proplists:get_value(Option, State#state.config).

fetch_url(Url, Command, State) ->
    Http_header = get_config(http_header, State),
    Request_options = get_config(request_options, State),
    Http_options = get_config(http_options, State),
    error_logger:info_report({?MODULE, ?LINE, {fetch_url, Command, Url}}),
    try 
	ebot_web_util:fetch_url(Url, Command, Http_header,Http_options,Request_options)
    catch
	Reason -> 
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, cannot_fetch_url, Reason}}),
	    {error, Reason}
    end.

fetch_url_links(Url, State) ->
    case fetch_url(Url, get, State) of
	{ok, {_Status, _Headers, Body}} ->
	    error_logger:info_report({?MODULE, ?LINE, {retreiving_links_from_body_of_url, Url}}),
	    {ok, ebot_html_util:get_links(Body, Url)};
	{error, Reason} ->
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url_links, Url, error, Reason}}),
	    {error, Reason};
	Other ->
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url_links, Url, unknown_error, Other}}),
	    Other
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
	    
start_crawler(Depth, State) ->
    Pid = spawn( ?MODULE, crawl, [Depth, State]),
    {Depth, Pid}.

start_crawlers(State) ->
    Pools = get_config(crawler_pools, State),
    
    NewCrawlers = lists:foldl(
		    fun({Depth,Tot}, Crawlers) ->
			    OtherCrawlers = start_crawlers(Depth, Tot, State),
			    lists:append( Crawlers, OtherCrawlers)
		    end,
		    State#state.crawlers_list,
		    Pools),
    NewState = State#state{
		 crawlers_list = NewCrawlers,
		 crawlers_status = started
		},
    NewState.

start_crawlers(Depth, Total, State) -> 
    lists:map(
      fun(_) -> start_crawler(Depth, State) end,
      lists:seq(1,Total)).
