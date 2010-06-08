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
%%% File    : ebot_setup_couchdb.erl
%%% Author  : matteo <matteo.redaelli@libero.it>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo 
%%%-------------------------------------------------------------------
-module(ebot_setup_couchdb).

%% API

-export([setup/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%% {
%%    "_id": "_design/ebot_domain",
%%    "_rev": "12-41054c61defc239c03237ac9dca8cac9",
%%    "language": "javascript",
%%    "views": {
%%        "get_domain_urls": {
%%            "map": "function (doc) {if (doc.ebot_doctype == \"url\") { emit(doc.ebot_domain, null);}}"
%%        },
%%        "get_domains": {
%%            "map": "function (doc) {if (doc.ebot_doctype == \"url\") { emit(doc.ebot_domain, 1);}}",
%%            "reduce": "function(keys, values) { return sum(values) }"
%%        },
%%        "get_main_domains": {
%%            "map": "function (doc) {if (doc.ebot_doctype == \"url\") { var url = doc.ebot_domain; var protocol_domain = url.split(\"://\"); var domain = protocol_domain[1].split(\".\"); var ext = domain.pop(); var dom = domain.pop(); emit(protocol_domain[0] + \"://\" + dom + \".\" + ext, 1)}}",
%%            "reduce": "function(keys, values) { return sum(values) }"
%%        }
%%    }
%% }

setup() ->
    create_views().
 
%%====================================================================
%% Internal functions
%%====================================================================

create_views() ->
    Doc1 = {[
	     {<<"_id">>, <<"_design/ebot_domain">>},
	     {<<"language">>,<<"javascript">>},
	     {<<"views">>,
	      {[
		%% view
		{<<"get_domain_urls">>,
		 {[
		   {<<"map">>,
		    <<"function (doc) {if (doc.ebot_doctype == \"url\") { emit(doc.ebot_domain, null);}}">>
		   }
		  ]}
		},
		%% view
		{<<"get_domains">>,
		 {[
		   {<<"map">>,
		    <<"function (doc) {if (doc.ebot_doctype == \"url\") { emit(doc.ebot_domain, 1);}}">>
		   },
		   {<<"reduce">>,
		    <<"function(keys, values) {return sum(values); }">> 
		   }
		  ]}
		},
		{<<"get_main_domains">>,
		 {[
		   {<<"map">>,
		    <<"function (doc) {if (doc.ebot_doctype == \"url\") { var url = doc.ebot_domain; var protocol_domain = url.split(\"://\"); var domain = protocol_domain[1].split(\".\"); var ext = domain.pop(); var dom = domain.pop(); emit(protocol_domain[0] + \"://\" + dom + \".\" + ext, 1)}}">>
		   },
		   {<<"reduce">>,
		    <<"function(keys, values) {return sum(values); }">> 
		   }
		  ]}
		}
	       %% end views
	       ]}
	     }]}, 		  
    ebot_db:create_view(Doc1).
