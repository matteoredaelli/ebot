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
%% EBOT_DB_BACKEND
%% 
%% VALID values are
%%            ebot_db_backend_couchdb
%%            ebot_db_backend_riak_pb

%% -----------------------------------------------------------------------
%% DB
%% -----------------------------------------------------------------------
%-define(EBOT_DB_BACKEND, ebot_db_backend_couchdb).
-define(EBOT_DB_BACKEND, ebot_db_backend_riak_pb).

%% -----------------------------------------------------------------------
%% WEB
%% -----------------------------------------------------------------------
-define(EBOT_WEB_TIMEOUT, 5000).

-define(EBOT_BODY_ANALYZER_PLUGINS, [
				     %% for saving TITLE, ...
				     {ebot_html_analyzer_header,add_header_tags},
				     {ebot_html_analyzer_images, add_images_list}
				     %,{ebot_html_analyzer_sample,analyze_url_body}
				    ]).

%% -----------------------------------------------------------------------
%% WORKERS
%% -----------------------------------------------------------------------
-define(EBOT_WORKER_TYPES, [html, web]).
-define(EBOT_EMPTY_QUEUE_TIMEOUT, 10000).
