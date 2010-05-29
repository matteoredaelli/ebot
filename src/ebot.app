{application, ebot,
 [{description, "ebot"},
  {vsn, "0.3"},
  {modules, [
    ebot,
    ebot_app,
    ebot_sup,
    ebot_deps,
    ebot_resource
  ]},
  {registered, []},
  {mod, {ebot_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
