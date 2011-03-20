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
%%% File    : ebot_db_doc_url.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_db_doc_url).

-include("ebot.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	 create_doc/1,
	 is_html_doc/1,
	 update_doc/2,
	 url_status/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
    
create_doc(Url) ->
    Domain = ebot_url_util:url_domain(Url),
    Doc = dict:from_list([ 
			   {<<"content_type">>, <<"">>},
			   {<<"ebot_body_visited">>, 0},
			   {<<"ebot_domain">>, list_to_binary(Domain)},
			   {<<"ebot_errors_count">>, 0},
			   {<<"ebot_head_visited">>, 0},
			   {<<"ebot_links_count">>, 0},
			   {<<"ebot_referrals">>, []},
			   {<<"ebot_visits_count">>, 0},
			   {<<"http_returncode">>,0}
			 ]),
    {ok, Doc}.

is_html_doc(Doc) ->
    Contenttype = ebot_db_doc:doc_get_value(<<"content_type">>, Doc),
    case re:run(Contenttype, "^text/html",[{capture, none},caseless] ) of
	match ->
	    true;
	nomatch ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: update_doc
%% Description: update a url doc using the options set by crawlers
%%--------------------------------------------------------------------
  
update_doc(Doc, {head, Headers, Keys}) ->
    update_url_head_doc(Doc, Headers, Keys);
update_doc(Doc, Unknown) ->
    error_logger:error_report({?MODULE, ?LINE, {update_doc, unknown_option, Unknown}}),
    {ok, Doc}.

%%--------------------------------------------------------------------
%% Function: update_url_head_doc
%% Description: update a doc url using the output of http:get(url, head)
%%--------------------------------------------------------------------

update_url_head_doc(Doc, Headers, Header_keys ) ->	    
    Doc2 = lists:foldl(
	     fun(BKey, Document) ->
		     Value = proplists:get_value(
			       binary_to_list(BKey),
			       Headers,
			       ""),
		     %% couchdb doesn't like - characters, so we convert it to _
		     NewBKey = list_to_binary(re:replace(binary_to_list(BKey), "-", "_", [global, {return,list}])),
		     error_logger:info_report({?MODULE, ?LINE, {update_url_head_doc, NewBKey, Value}}),
		     BValue = ebot_util:safe_list_to_binary(Value),
		     ebot_db_doc:update_doc(Document, [{update_value, NewBKey, BValue}])
	     end,
	     Doc,
	     Header_keys
	    ),
    NewOptions = [
		  %% {update_value, <<"http_returncode">>, Http_returncode},
		  {update_value, <<"ebot_head_error">>, <<"">>}
		 ],
    ebot_db_doc:update_doc(Doc2, NewOptions).

url_status(Db, Url, Options) ->
    case ebot_db_util:open_doc(Db, Url) of
	{ok, Doc} ->
	    url_doc_status(Doc, Options);
	Else ->
	    Else
    end.

%%====================================================================
%% Internal functions
%%====================================================================

url_doc_header_status(Doc, Options) ->
    Result = ebot_db_doc:doc_date_field_status(Doc, <<"ebot_head_visited">>, Options),
    {header, Result}.

url_doc_body_status(Doc, Options) ->
    case is_html_doc(Doc) of
	false ->
	    {body, skipped};
	true ->
	    Result = ebot_db_doc:doc_date_field_status(Doc, <<"ebot_body_visited">>, Options),
	    {body, Result}
    end.

%url_doc_status(not_found, _Options) ->
%    not_found;
url_doc_status(Doc, Options) ->
    HeaderStatus = url_doc_header_status(Doc, Options),
    BodyStatus = url_doc_body_status(Doc, Options),
    {ok, HeaderStatus, BodyStatus}.


-ifdef(TEST).

ebot_db_doc_url_test() ->
    ?assertNot(false == true).
 
    Headers = [{"cache-control","max-age=3600"},
		      {"connection","Keep-Alive"},
		      {"date","Sun, 16 Jan 2011 18:43:40 GMT"},
		      {"accept-ranges","bytes"},
		      {"etag","\"c227e5-a79e-499fa9440efc0\""},
		      {"server",
		       "Apache/2.3.8 (Unix) mod_ssl/2.3.8 OpenSSL/1.0.0a"},
		      {"vary","Accept-Encoding"},
		      {"content-length","42910"},
		      {"content-type","text/html"},
		      {"expires","Sun, 16 Jan 2011 19:43:40 GMT"},
		      {"last-modified",
		       "Sun, 16 Jan 2011 18:10:15 GMT"},
		      {"keep-alive","timeout=5, max=99"}]
	       [<<"server">>]},

    {ok, Doc3} = update_doc(Doc, {head, Headers, [<<"server">>]}).

-endif.
