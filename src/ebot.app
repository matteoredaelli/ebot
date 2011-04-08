%% -*- mode: erlang -*-

{application, ebot,
 [{description, "ebot"},
  {vsn, "1.0-snapshot"},
  {modules, [
    ebot,
    ebot_app,
    ebot_sup,
    ebot_deps,
    ebot_resource,

    ebot_crawler,
    ebot_db,
    ebot_db_backend_riak_pb,
    ebot_db_backend_couchdb,
    ebot_db_util,
    ebot_html,
    ebot_html_util,
    ebot_mq,
    ebot_stats,
    ebot_url_util,
    ebot_util,
    ebot_web,
    ebot_web_util,
    ebot_worker_util
  ]},
  {registered, []},
  {mod, {ebot_app, []}},
  {env, [

	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 %% EBOT config
	 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	 %% ---------------------------------------------------------
	 %% CACHE
	 %% ---------------------------------------------------------
	 {cache_new_urls_queue_size, 1000},
	 {cache_visited_urls_queue_size, 1000},
	 %% ---------------------------------------------------------
	 %% DATABASE
	 %% ---------------------------------------------------------
	 %% Remember also to set DB_BACKEND_MODULE in src/ebot.hrl

	 {db_hostname, "127.0.0.1"},
	 %% {db_port, 5984}, %% CouchDB
	 {db_port, 8087}, %% Riak

	 %% ---------------------------------------------------------
	 %% MQ
	 %% ---------------------------------------------------------
	 {mq_connection_params,
	  [
	   {username         , <<"guest">>},
	   {password          , <<"guest">>},
	   {virtual_host      , <<"/">>},
	   {host              , "localhost"},
	   {channel_max       , 0}
	  ]},
	 {mq_durable_queues, false},
	 {mq_priority_url_queues, 4},
	 
	 %% {mq_url_priority_plugin, {Module, Fun/1}}
	 %% Fun:
	 %%   input: url
	 %%   output: N >= 0, N=0 means highest priority
	 %%
	 %% The default function is url_depth that return the depth os thepath of the url
	 {mq_url_priority_plugin, {ebot_url_util, url_depth}},

	 %% ---------------------------------------------------------
	 %% WEB
	 %% ---------------------------------------------------------

	 {web_http_header, [
			    {"User-Agent", "Mozilla/5.0 ebot/1.0-snapshot"},
			    {"Accept-Charset", "utf-8"},
			    {"Accept", "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8"},
			    {"Accept-Language", "en-us,en;q=0.5"}
			   ]},
	 {web_http_options, [
			     {autoredirect, true}, 
			     {timeout, 10000}
			    ]},
	 {web_request_options, [
			        %{proxy, {{"proxy.mycompany.com", 80}, ["localhost"]}}
				%{proxy, noproxy} % erlang releases < R14A 
			       ]}
	 %% ---------------------------------------------------------
	 %% CRAWLER
	 %% ---------------------------------------------------------
	 
	 %% TODO: default parameters should be here, 
	 %% I'll copy them from ebot_local.config when ebot development
	 %% will be more quiet... 

	 %% END ENV
	]
  }, %% end env
  {applications, [kernel, stdlib, crypto, inets]}]}.

