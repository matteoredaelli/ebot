%% -*- mode: erlang -*-

{application, ebot,
 [{description, "ebot"},
  {vsn, "0.3"},
  {modules, [
    ebot,
    ebot_app,
    ebot_sup,
    ebot_deps,
    ebot_resource,
    ebot_db,
    ebot_cache,
    ebot_mq,
    ebot_web
  ]},
  {registered, []},
  {mod, {ebot_app, []}},
  {env, [
	 %% ---------------------------------------------------------
	 %% CACHE
	 %% ---------------------------------------------------------
	 {cache_new_urls_queue_size, 1000},
	 {cache_visited_urls_queue_size, 1000},
	 %% ---------------------------------------------------------
	 %% DATABASE
	 %% ---------------------------------------------------------
	 {db_hostname, "127.0.0.1"},
	 %% COUCHDB
	 {db_port, 5984},
	 %% RIAK 
	 %% {db_port, 8087}

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
	 {mq_tot_new_urls_queues, 4}
	 %% ---------------------------------------------------------
	 %% WEB
	 %% ---------------------------------------------------------


	 %% END ENV
	]
  }, %% end env
  {applications, [kernel, stdlib, crypto]}]}.
