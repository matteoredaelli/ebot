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
%%% File    : ebot_db_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_db_util).

-include("ebot.hrl").

%% API
-export([
	 create_url/2,
	 is_html_doc/1,
	 open_url/2,
	 open_or_create_url/2,
	 update_url/3,
	 url_status/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
    
create_url(Db, Url) ->
    Domain = ebot_url_util:url_domain(Url),
    Doc = dict:from_list([
	    {<<"http_returncode">>,0},
	    {<<"content_type">>, <<"">>},
	    {<<"ebot_body_visited">>, 0},
	    {<<"ebot_head_visited">>, 0},
	    {<<"ebot_domain">>, list_to_binary(Domain)},
	    {<<"ebot_errors_count">>, 0},
	    {<<"ebot_links_count">>, 0},
	    {<<"ebot_referrals">>, <<"">>},
	    {<<"ebot_referrals_count">>, 0},
	    {<<"ebot_visits_count">>, 0}
	   ]),
    Doc2 = update_url_doc(Doc, [{update_field_timestamp, <<"ebot_created_at">>}]),
    ?EBOT_DB_BACKEND:save_url_doc(Db, Url, Doc2).

is_html_doc(Doc) ->
    Contenttype = doc_get_value(<<"content_type">>, Doc),
    case re:run(Contenttype, "^text/html",[{capture, none},caseless] ) of
	match ->
	    true;
	nomatch ->
	    false
    end.

open_url(Db, Id) when is_list(Id) ->
    open_url(Db, list_to_binary(Id));
open_url(Db, Id) ->
    ?EBOT_DB_BACKEND:open_url(Db, Id).

open_or_create_url(Db, Url) ->
    case Doc = open_url(Db, Url) of
	not_found ->
	    create_url(Db, Url);
	Doc ->
	    Doc
    end.

update_url(Db, Url, Options) ->
    error_logger:info_report({?MODULE, ?LINE, {update_url, Url, with_options, Options}}),
    Doc = open_or_create_url(Db, Url),
    NewDoc = update_url_doc(Doc, Options),
%    error_logger:info_report({?MODULE, ?LINE, {update_url, Url, saving_doc, dict:to_list(NewDoc)}}),
    ?EBOT_DB_BACKEND:save_url_doc(Db, Url, NewDoc).

%%--------------------------------------------------------------------
%% Function: update_url_doc
%% Description: update a url doc using the options set by crawlers
%%--------------------------------------------------------------------
  
update_url_doc(Doc, [{referral, RefUrl}|Options]) ->
    %% TODO 
    %% managing more than one referral: we need also a job that
    %% periodically checks referrals...
    ReferralsCount = doc_get_value(<<"ebot_referrals_count">>, Doc),
    OldReferralsString = doc_get_value(<<"ebot_referrals">>, Doc),
  
    OldReferrals = re:split(OldReferralsString, " "),
    case lists:member( RefUrl, OldReferrals) of
	true ->
    	    NewReferrals = OldReferrals;
	false ->
	    NewReferrals = [RefUrl|OldReferrals]
    end,
    NewReferralsList = lists:map(fun binary_to_list/1, NewReferrals),
    NewReferralsString = string:join(NewReferralsList, " "),
    NewDoc = update_doc_by_key_value(Doc, <<"ebot_referrals">>, list_to_binary(NewReferralsString)),
    NewDoc2 = update_doc_by_key_value(NewDoc, <<"ebot_referrals_count">>, ReferralsCount + 1),
    update_url_doc(NewDoc2, Options);
update_url_doc(Doc, [{update_field_counter, Key}|Options]) ->
    NewDoc = update_doc_increase_counter(Doc, Key),
    update_url_doc(NewDoc, Options);
update_url_doc(Doc, [{update_field_timestamp, Key}|Options]) ->
    NewDoc = update_doc_timestamp_by_key(Doc, Key),
    update_url_doc(NewDoc, Options);
update_url_doc(Doc, [{update_field_key_value, Key, Value}|Options]) ->
    NewDoc = update_doc_by_key_value(Doc, Key, Value),
    update_url_doc(NewDoc, Options);
update_url_doc(Doc, [{head, Result, Keys}|Options]) ->
    NewDoc = update_url_head_doc(Doc, Result, Keys),
    update_url_doc(NewDoc, Options);
update_url_doc(Doc, []) ->
    Doc.

%%--------------------------------------------------------------------
%% Function: update_url_head_doc
%% Description: update a doc url using the output of http:get(url, head)
%%--------------------------------------------------------------------

update_url_head_doc(Doc, {{_,Http_returncode,_}, Headers, _Body}, Header_keys ) ->	    
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
		     doc_set_value(
		       NewBKey, 
		       BValue,
		       Document)
	     end,
	     Doc,
	     Header_keys
	    ),
    Doc3 = doc_set_value( <<"http_returncode">>, Http_returncode, Doc2),
    update_doc_by_key_value(Doc3, <<"ebot_head_error">>, <<"">>).

url_status(Db, Url, Options) ->
    Doc = open_url(Db, Url),
    url_doc_status(Doc, Options).

%%====================================================================
%% Internal functions
%%====================================================================

date_field_status(Date, Days) ->
    Now = calendar:datetime_to_gregorian_seconds( calendar:universal_time() ),
    Diff = Now - Date, 
    case Diff > Days * 86400 of
	true ->
	    obsolete;
	false ->
	    updated
    end.

doc_date_field_status(Doc, Field, Days) ->
    case Date = doc_get_value(Field, Doc) of
	0 ->
	    new;
	Date ->
	    date_field_status(Date, Days)
    end.   

doc_get_value(Key, Doc) ->
    case dict:find(Key, Doc) of
	{ok, Value} ->
	    Value;
	error ->
	    error
    end.

doc_set_value(Key, Value, Doc) ->
    error_logger:info_report({?MODULE, ?LINE, {doc_set_value, Key, Value, Doc}}),
    dict:store(Key, Value, Doc).

%% removing_db_stardard_keys(Keys) ->
%%     lists:filter(
%%       fun(Key) ->
%% 	      case re:run(Key,<<"^_">>) of
%% 		  {match, _} ->
%% 		      false;
%% 		  nomatch ->
%% 		      true
%% 	      end
%%       end,
%%       Keys).

update_doc_increase_counter(Doc, Key) ->
    Value = doc_get_value(Key, Doc),
    update_doc_by_key_value(Doc, Key, Value + 1).

update_doc_timestamp_by_key(Doc, Key) ->
    Value = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    update_doc_by_key_value(Doc, Key, Value).

update_doc_by_key_value(Doc, Key, Value) ->
    doc_set_value(Key, Value, Doc).

url_doc_header_status(Doc, Options) ->
    Result = doc_date_field_status(Doc, <<"ebot_head_visited">>, Options),
    {header, Result}.

url_doc_body_status(Doc, Options) ->
    case is_html_doc(Doc) of
	false ->
	    {body, skipped};
	true ->
	    Result = doc_date_field_status(Doc, <<"ebot_body_visited">>, Options),
	    {body, Result}
    end.

url_doc_status(not_found, _Options) ->
    not_found;
url_doc_status(Doc, Options) ->
    HeaderStatus = url_doc_header_status(Doc, Options),
    BodyStatus = url_doc_body_status(Doc, Options),
    {ok, HeaderStatus, BodyStatus}.
