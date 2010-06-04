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
	 {db_port, 5984}
	 %% RIAK 
	 %% {db_port, 8087}.
	]
  },
  {applications, [kernel, stdlib, crypto]}]}.
