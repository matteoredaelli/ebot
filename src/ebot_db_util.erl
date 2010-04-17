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
	 is_obsolete_url/3,
	 open_doc/2,
	 open_or_create_url/2,
	 update_url/3
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
	    {<<"date">>,<<"Tue, 01 Jan 1980 00:00:00 GMT">>},
	    {<<"keep-alive">>, <<"">>},
	    {<<"last-modified">>, <<"">>},
	    {<<"server">>, <<"">>},
	    {<<"visited">>, 0},
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

    
update_url(Db, Url,{ok, {{_,Http_returncode,_}, Headers, _Body}} ) ->
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
update_url(_Db, _Url, _) ->
    error.

is_obsolete_url(Db, Url, Days) ->
    case Doc = open_doc(Db, Url) of
	not_found ->
	    Reply = not_found;
	Doc ->
	    Now = calendar:local_time(),
	    Date1 = httpd_util:convert_request_date(couchbeam_doc:get_value(<<"date">>, Doc)),
	    {Diff, _} = calendar:time_difference(Date1, Now),
	    if Diff > Days ->
		    Reply = obsolete;
	       true ->
		    Reply = ok
	    end % if
    end, % case
    Reply.

is_html_doc(Doc) ->
    Contenttype = couchbeam_doc:get_value(<<"content-type">>, Doc),
    case re:run(Contenttype, "^text/html") of
	{match, _} ->
	    true;
	nomatch ->
	    false
    end.

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

save_doc(Db, Doc) ->
    couchbeam_db:save_doc(Db, Doc).

removing_db_stardard_keys(Keys) ->
    lists:filter(
      fun(Key) ->
	      case re:run(Key,<<"^_">>) of
		  {match, _} ->
		      false;
		  nomatch ->
		      true
	      end
      end,
      Keys).

ebot_header_keys()->
    [
     <<"content-length">>,
     <<"content-type">>,
     <<"date">>,
     <<"last-modified">>,
     <<"server">>,
     <<"x-powered-by">>
    ].
