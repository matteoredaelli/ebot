%%%-------------------------------------------------------------------
%%% File    : ebot_db_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_db_util).

%% API
-export([
	 create_url/2,
	 is_html_doc/1,
	 open_doc/2,
	 open_or_create_url/2,
	 update_url_header/3,
	 update_url_body/2,
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
    Doc = {[
	    {<<"_id">>, Url},
	    {<<"http-returncode">>,0},
	    {<<"content-length">>, 0},
	    {<<"content-type">>, <<"">>},
	    {<<"date">>,<<"">>},
	    {<<"keep-alive">>, <<"">>},
	    {<<"last-modified">>, <<"">>},
	    {<<"server">>, <<"">>},
	    {<<"ebot-body-visited">>, <<"">>},
	    {<<"x-powered-by">>,<<"">>}
	   ]},
    create_doc(Db, Doc, <<"url">>).

open_doc(Db, Id) when is_list(Id) ->
    open_doc(Db, list_to_binary(Id));
open_doc(Db, Id) ->
    couchbeam_db:open_doc(Db, Id).

open_or_create_url(Db, Url) ->
    case Doc = open_doc(Db, Url) of
	not_found ->
	    create_url(Db, Url);
	Doc ->
	    Doc
    end.

    
update_url_header(Db, Url,{ok, {{_,Http_returncode,_}, Headers, _Body}} ) ->
    {Proplist} = open_doc(Db, Url), 
    Header_keys = ebot_header_keys(),	    
    NewDoc = lists:foldl(
	       fun(BKey, Doc) ->    
		       Value = proplists:get_value(
				 binary_to_list(BKey),
				 Headers,
				 ""),
		       BValue = ebot_util:safe_list_to_binary(Value),
		       couchbeam_doc:set_value(
			 BKey, 
			 BValue,
			 Doc)
	       end,
	       {Proplist},
	       Header_keys
	      ),
    NewDoc2 = couchbeam_doc:set_value(
		<<"http-returncode">>,
		Http_returncode,
		NewDoc),
    save_doc(Db, NewDoc2);
update_url_header(_Db, _Url, _) ->
    error.

update_url_body(Db, Url) ->
    update_url_timestamp_by_key(Db, Url, <<"ebot-body-visited">>).


is_html_doc(Doc) ->
    Contenttype = couchbeam_doc:get_value(<<"content-type">>, Doc),
    case re:run(Contenttype, "^text/html") of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.

url_status(Db, Url, Options) ->
    Doc = open_doc(Db, Url),
    url_doc_status(Doc, Options).

%%====================================================================
%% Internal functions
%%====================================================================

create_doc(Db, Doc, Type) ->
    Defaults = [
	       {<<"doctype">>, Type},
	       {<<"created_at">>, list_to_binary(httpd_util:rfc1123_date())}
	      ],
    NewDoc = lists:foldl(
	       fun({Key,Value}, Acc_doc) ->
		       couchbeam_doc:extend(Key, Value, Acc_doc) end,
	       Doc,
	       Defaults),
    save_doc(Db, NewDoc).

doc_date_field_status(Doc, Field, Days) ->
    case BinDate = couchbeam_doc:get_value(Field, Doc) of
	<<"">> ->
	    new;
	BinDate ->
	    Date = httpd_util:convert_request_date(binary_to_list(BinDate)),
	    date_field_status(Date, Days)
    end.

date_field_status(Date, Days) ->
    Now = calendar:local_time(),
    {Diff, _} = calendar:time_difference(Date, Now),
    case Diff > Days of
	true ->
	    obsolete;
	false ->
	    updated
    end.

ebot_header_keys()->
    [
     <<"content-length">>,
     <<"content-type">>,
     <<"date">>,
     <<"last-modified">>,
     <<"server">>,
     <<"x-powered-by">>
    ].

save_doc(Db, Doc) ->
    couchbeam_db:save_doc(Db, Doc).


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

update_url_timestamp_by_key(Db, Url, Key) ->
    Doc = open_doc(Db, Url),
    Value = list_to_binary(httpd_util:rfc1123_date()),
    update_doc_by_key_value( Db, Doc, Key, Value).

update_doc_by_key_value(Db, Doc, Key, Value) ->
    NewDoc = couchbeam_doc:set_value(Key, Value, Doc),
    save_doc(Db, NewDoc).

url_doc_header_status(Doc, Options) ->
    Result = doc_date_field_status(Doc, <<"date">>, Options),
    {header, Result}.

url_doc_body_status(Doc, Options) ->
    case is_html_doc(Doc) of
	false ->
	    {body, skipped};
	true ->
	    Result = doc_date_field_status(Doc, <<"ebot-body-visited">>, Options),
	    {body, Result}
    end.

url_doc_status(not_found, _Options) ->
    not_found;
url_doc_status(Doc, Options) ->
    HeaderStatus = url_doc_header_status(Doc, Options),
    BodyStatus = url_doc_body_status(Doc, Options),
    {ok, HeaderStatus, BodyStatus}.

    
