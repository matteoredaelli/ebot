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
%%% File    : ebot_html.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  19 Jun 2010 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_html).
-author("matteo.redaelli@libero.it").

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(WORKER_TYPE, html).

-include("ebot.hrl").

-behaviour(gen_server).

%% API
-export([
	 analyze_url/2,
	 analyze_url_body_plugins/2,
	 check_recover_workers/0,
	 run/1,
	 info/0,
	 remove_worker/1,
	 get_workers/0,
	 start_workers/0,
	 start_workers/2,
	 start_link/0,
	 statistics/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,{
	  workers = ebot_worker_util:create_workers(?WORKER_TYPE)
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
get_workers() ->
    gen_server:call(?MODULE, {get_workers}).
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
    case ebot_util:get_env(start_workers_at_boot) of
	{ok, true} ->
	    State = start_workers(#state{});
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
    Workers = ebot_worker_util:check_recover_workers(State#state.workers),
    NewState = State#state{
		 workers = Workers
		},
    {reply, Workers, NewState};

handle_call({info}, _From, State) ->
    {reply, ok, State};

handle_call({statistics}, _From, State) ->
    Reply = ebot_worker_util:statistics(State#state.workers),
    {reply, Reply, State};

handle_call({get_workers}, _From, State) ->
    {?WORKER_TYPE, Reply} = State#state.workers,
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
		 workers = ebot_worker_util:remove_worker(Worker, 
							  State#state.workers)
		},
    {noreply, NewState};

handle_cast({start_workers}, State) ->
    NewState = start_workers(State),
    {noreply, NewState};

handle_cast({start_workers, Depth, Tot}, State) ->
    NewState = State#state{
		 workers = ebot_worker_util:start_workers(Depth,Tot, 
							  State#state.workers)
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



run(Depth) ->
    case ebot_mq:receive_url_fetched(Depth) of
	{ok, {Url, Result}} ->
	    analyze_url(Url, Result);
	{error, _} ->
	    error_logger:info_report({?MODULE, ?LINE, {run, no_queued_urls, waiting}}),
	    timer:sleep( ?EBOT_EMPTY_QUEUE_TIMEOUT )
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

analyze_url(Url,{error, Reason}) ->
    error_logger:error_report({?MODULE, ?LINE, {analyze_url, Url, skipping_url, Reason}}),
    Options = [{update_counter, <<"ebot_errors_count">>},
	       {update_value, <<"ebot_head_error">>, list_to_binary(atom_to_list(Reason))}
	      ],

    %% TODO: instead of only Url, it would be nice to send {Url, Reason}
    %% like <<"https://github.com/login/,https_through_proxy_is_not_currently_supported">>
    ebot_mq:send_url_refused({Url,Reason}),
    ebot_db:update_doc(Url, Options),
    {error, Reason};

analyze_url(Url,{ok, {Status, Headers, Body}}) ->
    analyze_url_head(Url, {Status, Headers, empty}),
    analyze_url_body(Url, {empty, empty, Body}).

analyze_url_head(Url, Result = {_Status, _Headers, empty}) ->
    {ok, H} = ebot_util:get_env(tobe_saved_headers),
    Options = [{head, Result, H}, 
	       {update_timestamp,<<"ebot_head_visited">>},
	       {update_counter, <<"ebot_visits_count">>},
	       {update_value, <<"ebot_errors_count">>, 0}
	      ],
    ebot_db:update_doc(Url, Options).

analyze_url_body(Url, {_Status, _Headers, empty}) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body, Url, empty_body}});

analyze_url_body(Url, {_Status, _Headers, Body}) ->
    Tokens = mochiweb_html:tokens(Body),
    spawn(?MODULE, analyze_url_body_plugins, [Url, Tokens]),
    error_logger:info_report({?MODULE, ?LINE, {analyze_body_plugins, Url}}),
    Links = ebot_html_util:get_links_from_tokens(Tokens, Url),
    analyze_url_body_links(Url, Links),
    ebot_mq:send_url_processed({Url, empty}).

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
				(not ebot_crawler:is_visited_url(U)) andalso
				    ebot_url_util:is_valid_url(U)
			end,
			UniqueLinks),
    
    %% retrive Url from DB
    lists:foreach(
      fun(U) ->
	      %% creating the url in the database if it doen't exists
	      error_logger:info_report({?MODULE, ?LINE, {adding, U, from_referral, Url}}),
	      ebot_db:open_or_create_url(U),
	      ebot_crawler:add_new_url(U),
	      case  needed_update_url_referral(
		      ebot_url_util:is_same_main_domain(Url, U),
		      ebot_url_util:is_same_domain(Url, U)) of
		  true ->	    
		      Options = [{referral, Url}],
		      ebot_db:update_doc(U, Options);
		  false ->
		      ok
	      end
      end,
      NotVisitedLinks),
    %% UPDATE ebot-body-visited
    Options = [{update_timestamp, <<"ebot_body_visited">>},
	       {update_value, <<"ebot_links_count">>, LinksCount}
	      ],
    ebot_db:update_doc(Url, Options),
    ok.

analyze_url_body_plugins(Url, Tokens) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_plugins, Url}}),
    lists:foreach(
	     fun({Module, Function}) ->
		     analyze_url_body_plugin(Url, Tokens, Module, Function)
	     end,
	     ?EBOT_BODY_ANALYZER_PLUGINS
     ).

analyze_url_body_plugin(Url, Tokens, Module, Function) ->  
    Options = Module:Function(Url, Tokens),
    error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_plugin, Url, Module, Function, Options}}),
    ebot_db:update_doc(Url, Options).

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

start_workers(State) ->
    {ok, Pool} = ebot_util:get_env(workers_pool),
    State#state{
      workers = ebot_worker_util:start_workers(Pool, 
					       State#state.workers)
	  }.

%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_html_test() ->
     <<"http://www.redaelli.org/matteo/ebot_test/">>.

-endif.
