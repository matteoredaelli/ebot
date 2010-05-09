%%%-------------------------------------------------------------------
%%% File    : ebot_setup.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_setup).

%% API
-export([create_views/0]).

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

%%====================================================================
%% Internal functions
%%====================================================================
