%% EBOT_DB_BACKEND
%% 
%% VALID values are
%%            ebot_db_backend_couchdb
%%            ebot_db_backend_riak_pb

%% -----------------------------------------------------------------------
%% DB
%% -----------------------------------------------------------------------
-define(EBOT_DB_BACKEND, ebot_db_backend_couchdb).
%% -define(EBOT_DB_BACKEND, ebot_db_backend_riak_pb).

%% -----------------------------------------------------------------------
%% WEB
%% -----------------------------------------------------------------------
-define(EBOT_WEB_TIMEOUT, 5000).

-define(EBOT_BODY_ANALYZER_PLUGINS, [{ebot_plugin_body_analyzer_sample,analyze_url_body}]).

