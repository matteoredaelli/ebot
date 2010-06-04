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
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
