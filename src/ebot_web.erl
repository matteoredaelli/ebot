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
-define(WORKER_TYPE, web).

-include("ebot.hrl").

-behaviour(gen_server).

%% API
-export([
	 analyze_url/1,
	 analyze_url_body_plugins/2,
	 check_recover_workers/0,
	 run/1,
	 info/0,
	 remove_worker/1,
	 show_worker_list/0,
	 start_workers/0,
	 start_workers/2,
	 start_link/0,
	 statistics/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{
	  worker_list = ebot_worker_util:create_worker_list(?WORKER_TYPE)
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
check_recover_workers() ->
    gen_server:call(?MODULE, {check_recover_workers}).
info() ->
    gen_server:call(?MODULE, {info}).
show_worker_list() ->
    gen_server:call(?MODULE, {show_worker_list}).
start_workers() ->
    gen_server:cast(?MODULE, {start_workers}).
start_workers(Depth, Tot) ->
    gen_server:cast(?MODULE, {start_workers, Depth, Tot}).
statistics() ->
    gen_server:call(?MODULE, {statistics}).
remove_worker(Worker) ->
    gen_server:cast(?MODULE, {remove_worker, Worker}).

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
    case ebot_util:get_env(start_workers_at_boot) of
	{ok, true} ->
	    State = start_workers_pool(#state{});
	{ok, false} ->
	    State = #state{}
    end,
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
handle_call({check_recover_workers}, _From, State) ->
    NewState = State#state{
		 worker_list = ebot_worker_util:check_recover_workers(State#state.worker_list)
		},
    {reply, ok, NewState};

handle_call({info}, _From, State) ->
    {reply, ok, State};

handle_call({statistics}, _From, State) ->
    Reply = ebot_worker_util:statistics(State#state.worker_list),
    {reply, Reply, State};

handle_call({show_worker_list}, _From, State) ->
    {?WORKER_TYPE, Reply} = State#state.worker_list,
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
handle_cast({remove_worker, Worker}, State) ->
    NewState = State#state{
		 worker_list = ebot_worker_util:remove_worker(Worker, 
							      State#state.worker_list)
		},
    {noreply, NewState};

handle_cast({start_workers}, State) ->
    NewState = start_workers_pool(State),
    {noreply, NewState};

handle_cast({start_workers, Depth, Tot}, State) ->
    NewState = State#state{
		 worker_list = ebot_worker_util:start_workers(Depth,Tot, 
								   State#state.worker_list)
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

analyze_url(empty) ->
    {ok, empty};
analyze_url(Url) ->
    {ok, Days} = ebot_util:get_env(obsolete_urls_after_days),
    analyze_url_from_url_status(Url, ebot_db:url_status(Url, Days)).

analyze_url_header(Url) ->
    ebot_cache:add_visited_url(Url),
    case Result = ebot_web_util:fetch_url_head(Url) of
	{error, Reason} -> 
	    error_logger:error_report({?MODULE, ?LINE, {analyze_url_header, Url, skipping_url, Reason}}),
	    Options = [{update_field_timestamp, <<"ebot_errors_count">>},
		       {update_field_key_value, <<"ebot_head_error">>, list_to_binary(atom_to_list(Reason))}
		      ],
	    %% TODO: instead of only Url, it would be nice to send {Url, Reason}
	    %% like <<"https://github.com/login/,https_through_proxy_is_not_currently_supported">>
	    ebot_mq:send_url_refused({Url,Reason});
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
    case ebot_web_util:fetch_url_get(Url) of
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
	    ebot_mq:send_url_processed({Url, NewBody});
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
				{ok, ListNormalizeOptions} = ebot_util:get_env(normalize_url),
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
				    ebot_url_util:is_valid_url(U)
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

get_env_normalize_url(Url, [{RE,Options}|L]) ->
    case re:run(Url, RE, [{capture, none},caseless]) of
	match ->
	    {ok, Options};
	nomatch ->
	    get_env_normalize_url(Url, L)
    end;
get_env_normalize_url(_Url, []) ->
    undefined.

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

run(Depth) ->
    case ebot_mq:receive_url_new(Depth) of
	{ok, {Url, _}} ->
	    analyze_url(Url);
	{error, _} ->
	    timer:sleep( 2000 )
    end,
    case ebot_crawler:workers_status() of
	started ->
	    {ok, Sleep} = ebot_util:get_env(workers_sleep_time),
	    timer:sleep( Sleep ),
	    run(Depth);
	stopped ->
	    error_logger:warning_report({?MODULE, ?LINE, {stopping_worker, self()}}),
	    remove_worker( {Depth, self()} )
    end.

start_workers_pool(State) ->
    {ok, Pool} = ebot_util:get_env(workers_pool),
    State#state{
	    worker_list = ebot_worker_util:start_workers_pool(Pool, 
							      State#state.worker_list)
	  }.

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
    UrlLinks = ebot_web_util:fetch_url_links(Url),
    ?assertEqual( {ok, ExpectedUrlLinks}, UrlLinks),
    ExpectedExternalLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>],
    ?assertEqual(ExpectedExternalLinks,  
		 ebot_url_util:filter_external_links(Url, ExpectedUrlLinks)).
-endif.
