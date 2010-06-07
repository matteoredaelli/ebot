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

-include("ebot.hrl").

-behaviour(gen_server).

%% API
-export([
	 analyze_url/1,
	 analyze_url_body_plugins/2,
	 check_recover_crawlers/0,
	 crawl/1,
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
	  crawlers_status = started,  %% or stopped
	  crawlers_list = []
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
    {ok, Options} = ebot_util:get_env(web_request_options),
    http:set_options(Options),
    State = #state{},
    case ebot_util:get_env(start_crawlers_at_boot) of
	{ok, true} ->
	    Crawlers_status = started,
	    NewState = start_crawlers(State);
	{ok, false} ->
	    Crawlers_status = stopped,
	    NewState = State
    end,
    {ok, NewState#state{crawlers_status = Crawlers_status}}.

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
    Reply = try_fetch_url(Url, Command),
    {reply, Reply, State};
handle_call({fetch_url_links, Url}, _From, State) ->
    Reply = try_fetch_url_links(Url),
    {reply, Reply, State};
handle_call({info}, _From, State) ->
    Crawlers = State#state.crawlers_list,
    Reply = lists:map(
	      fun({Depth, Pid}) ->
		      {Depth, process_info(Pid)}
	      end,
	      Crawlers),
    {reply, Reply, State};
handle_call({statistics}, _From, State) ->
    {ok, Pools} = ebot_util:get_env(crawler_pools),
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

analyze_url(empty) ->
    {ok, empty};
analyze_url(Url) ->
    {ok, Days} = ebot_util:get_env(obsolete_urls_after_days),
    analyze_url_from_url_status(Url, ebot_db:url_status(Url, Days)).

analyze_url_header(Url) ->
    ebot_cache:add_visited_url(Url),
    case Result = try_fetch_url(Url, head) of
	{error, Reason} -> 
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_header, Url, skipping_url, Reason}}),
	    Options = [{update_field_key_value, <<"ebot_errors_count">>, 0},
		       {update_field_key_value, <<"ebot_head_error">>, list_to_binary(atom_to_list(Reason))}
		      ],
	    %% TODO: instead of only Url, it would be nice to send {Url, Reason}
	    %% like <<"https://github.com/login/,https_through_proxy_is_not_currently_supported">>
	    ebot_mq:add_refused_url( term_to_binary({Url,Reason}, [compressed]));
	Result ->
	    {ok, H} = ebot_util:get_env(tobe_saved_headers),
	    Options = [{head, Result, H}, 
		       {update_field_timestamp,<<"ebot_head_visited">>},
		       {update_field_counter, <<"ebot_visits_count">>},
		       {update_field_key_value, <<"ebot_errors_count">>, 0}
		      ]
    end,
    ebot_db:update_url(Url, Options),
    Result.

analyze_url_body(Url) ->
    case try_fetch_url(Url, get) of
	{ok, {_Status, _Headers, Body}} ->
	    spawn(?MODULE, analyze_url_body_plugins, [Url, Body]),
	    error_logger:info_report({?MODULE, ?LINE, {analyze_body_plugins, Url}}),
	    Links = ebot_html_util:get_links(Body, Url),
	    analyze_url_body_links(Url, Links),
	    {ok, SendBodyToMqOption} = ebot_util:get_env(send_body_to_mq),
	    case SendBodyToMqOption of
		true ->
		    NewBody= list_to_binary(Body);
		false ->
		    NewBody = <<>>
	    end,
	    Payload = term_to_binary({Url, NewBody}, [compressed]),
	    ebot_mq:add_processed_url(Payload);
	{error, Reason} ->
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url_links, Url, error, Reason}}),
	    {error, Reason};
	Other ->
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url_links, Url, unknown_error, Other}}),
	    Other
    end.

analyze_url_body_links(Url, Links) ->
    error_logger:info_report({?MODULE, ?LINE, {getting_links_of, Url}}),
    LinksCount = length(ebot_url_util:filter_external_links(Url, Links)),
    %% normalizing Links
    error_logger:info_report({?MODULE, ?LINE, {normalizing_links_of, Url}}),
    NormalizedLinks = lists:map(
			fun(U) -> 
				{ok, ListNormalizeOptions} = ebot_util:get_env(normalize_url_list),
				{ok, NormalizeOptions} = get_env_normalize_url(Url, ListNormalizeOptions),
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
				(not ebot_cache:is_visited_url(U)) andalso
				    is_valid_url(U)
			end,
			UniqueLinks),
    
    %% retrive Url from DB
    lists:foreach(
      fun(U) ->
	      %% creating the url in the database if it doen't exists
	      error_logger:info_report({?MODULE, ?LINE, {adding, U, from_referral, Url}}),
	      ebot_db:open_or_create_url(U),
	      ebot_cache:add_new_url(U),
	      case  needed_update_url_referral(
		      ebot_url_util:is_same_main_domain(Url, U),
		      ebot_url_util:is_same_domain(Url, U)) of
		  true ->	    
		      Options = [{referral, Url}],
		      ebot_db:update_url(U, Options);
		  false ->
		      ok
	      end
      end,
      NotVisitedLinks),
    %% UPDATE ebot-body-visited
    Options = [{update_field_timestamp, <<"ebot_body_visited">>},
	       {update_field_key_value, <<"ebot_links_count">>, LinksCount}
	      ],
    ebot_db:update_url(Url, Options),
    ok.

analyze_url_body_plugins(Url, Body) ->
    Options = lists:foldl(
	     fun({Module, Function}, OptList) ->
		     error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_plugins, Url, Module, Function}}),
		     lists:append(Module:Function(Body, Url), OptList) end,
	     [],
	     ?EBOT_BODY_ANALYZER_PLUGINS
	    ),
    ebot_db:update_url(Url, Options).

analyze_url_from_url_status(Url, not_found) ->
    ebot_db:open_or_create_url(Url),
    analyze_url(Url);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,new}}) ->
    analyze_url_body(Url);
analyze_url_from_url_status(Url, {ok, {header, updated}, {body,obsolete}}) ->
    analyze_url_body(Url);
analyze_url_from_url_status(_Url, {ok, {header, updated}, {body, skipped}}) ->
    ok;
analyze_url_from_url_status(_Url, {ok, {header, updated}, {body, updated}}) ->
    ok;
%% HeaderStatus  can be: new or obsolete
analyze_url_from_url_status(Url, {ok, {header, HeaderStatus}, {body,_}}) ->
    error_logger:warning_report({?MODULE, ?LINE, {analyze_url_from_url_status, Url, header_status, HeaderStatus }}),
    case analyze_url_header(Url) of
	{error, Reason} ->
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_from_url_status, Url, error, Reason}}),
	    {error, Reason};
	{ok, _} ->
	    %% not sure if the url is html or not and so coming back 
	    %% to check url_status
	    analyze_url(Url);
	Other ->
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_from_url_status, Url, unexpected_result, Other}}),
	    Other
    end.

check_recover_crawlers(State) ->
    Crawlers = State#state.crawlers_list,
    lists:map( 
      fun({Depth, Pid}) ->
	      case erlang:is_process_alive(Pid) of
		  true ->
		      error_logger:warning_report({?MODULE, ?LINE, 
					   {check_recover_crawlers, status,
					    proplists:get_value(status, process_info(Pid)) }}),
		      {Depth, Pid};
		  false ->
		      error_logger:warning_report({?MODULE, ?LINE, {check_recover_crawlers, recovering_dead_crawler}}),
		      start_crawler(Depth)
	      end
      end,
      Crawlers).

crawl(Depth) ->
    Url = ebot_mq:get_new_url(Depth),
    analyze_url(Url),
    case ebot_web:crawlers_status() of
	started ->
	    {ok, Sleep} = ebot_util:get_env(crawlers_sleep_time),
	    timer:sleep( Sleep ),
	    crawl(Depth);
	stopped ->
	    error_logger:warning_report({?MODULE, ?LINE, {stopping_crawler, self()}}),
	    stop_crawler( {Depth, self()} )
    end.

get_env_normalize_url(Url, [{RE,Options}|L]) ->
    case re:run(Url, RE, [{capture, none},caseless]) of
	match ->
	    {ok, Options};
	nomatch ->
	    get_env_normalize_url(Url, L)
    end;
get_env_normalize_url(_Url, []) ->
    undefined.

is_valid_url(Url) when is_binary(Url) ->
    is_valid_url(binary_to_list(Url));

is_valid_url(Url) ->
    {ok, MimeAnyRE} =  ebot_util:get_env(mime_any_regexps),
    {ok, UrlAllRE} =  ebot_util:get_env(url_all_regexps),
    {ok, UrlAnyRE} = ebot_util:get_env(url_any_regexps),
    ebot_util:is_valid_using_all_regexps(Url, UrlAllRE) andalso
	ebot_util:is_valid_using_any_regexps(Url, UrlAnyRE) andalso
	ebot_url_util:is_valid_url_using_any_mime_regexps(Url, MimeAnyRE).

needed_update_url_referral(SameMainDomain, SameDomain) ->
    {ok, SaveReferralsOptions} = ebot_util:get_env(save_referrals),
    lists:any(
      fun(Option) -> needed_update_url_referral(SameMainDomain, SameDomain, Option) end,
      SaveReferralsOptions
     ).

%% needed_update_url_referral(SameMainDomain, SameDomain, domain|subdomain|external)

needed_update_url_referral(false, false, external) ->	    
    true;
needed_update_url_referral(true, false, subdomain) ->	    
    true;
needed_update_url_referral(_, true, domain) ->
    true;
needed_update_url_referral(_,_,_) ->
    false.

start_crawler(Depth) ->
    Pid = spawn( ?MODULE, crawl, [Depth]),
    {Depth, Pid}.

start_crawlers(State) ->
    {ok, Pools} = ebot_util:get_env(crawler_pools),
    
    NewCrawlers = lists:foldl(
		    fun({Depth,Tot}, Crawlers) ->
			    OtherCrawlers = start_crawlers(Depth, Tot),
			    lists:append( Crawlers, OtherCrawlers)
		    end,
		    State#state.crawlers_list,
		    Pools),
    NewState = State#state{
		 crawlers_list = NewCrawlers,
		 crawlers_status = started
		},
    NewState.

start_crawlers(Depth, Total) -> 
    lists:map(
      fun(_) -> start_crawler(Depth) end,
      lists:seq(1,Total)).


try_fetch_url(Url, Command) ->
    {ok, Http_header} = ebot_util:get_env(web_http_header),
    {ok, Request_options} = ebot_util:get_env(web_request_options),
    {ok, Http_options} = ebot_util:get_env(web_http_options),
    error_logger:info_report({?MODULE, ?LINE, {fetch_url, Command, Url}}),
    try 
	ebot_web_util:fetch_url(Url, Command, Http_header,Http_options,Request_options)
    catch
	Reason -> 
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, cannot_fetch_url, Reason}}),
	    {error, Reason}
    end.

try_fetch_url_links(Url) ->
    case fetch_url(Url, get) of
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

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_web_test() ->
    Url =  <<"http://www.redaelli.org/matteo/ebot_test/">>,
    ExpectedUrlLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>,
			     <<"http://www.redaelli.org/">>,
			     <<"http://www.redaelli.org/matteo">>,
			     <<"http://www.redaelli.org/matteo/ebot_test/dir1">>
		       ],
    UrlLinks = try_fetch_url_links(Url),
    ?assertEqual( {ok, ExpectedUrlLinks}, UrlLinks),
    ExpectedExternalLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>],
    ?assertEqual(ExpectedExternalLinks,  
		 ebot_url_util:filter_external_links(Url, ExpectedUrlLinks)).
-endif.
