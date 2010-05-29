%%%-------------------------------------------------------------------
%%% File    : ebot_db_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_db_backend_couchdb).

%% API
-export([
	 open_url/2,
	 save_url_doc/3,
	 statistics/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

open_url(Db, Id) ->
    open_doc(Db, Id).

save_url_doc(Db, Url, Doc) ->
    case dict:find(<<"_id">>, Doc) of
	error ->
	    NewDoc =  dict:store(<<"_id">>, Url, Doc);
	_ ->
	    NewDoc = Doc
    end,
    save_doc(Db, NewDoc).

statistics(Db) ->
    Doc = couchbeam_db:info(Db),
    DiskSize = round(proplists:get_value(<<"disk_size">>, Doc) / 1024 / 1024),
    DocCount = proplists:get_value(<<"doc_count">>, Doc),
    [{<<"disk_size">>, DiskSize}, {<<"doc_count">>,DocCount}].

%%====================================================================
%% Internal Functions
%%====================================================================

open_doc(Db, Id) ->
    case couchbeam_db:open_doc(Db, Id) of
	not_found ->
	    not_found;
	{Doc} ->
	    dict:from_list(Doc)
    end.

save_doc(Db, Doc) ->
    couchbeam_db:save_doc(Db, {dict:to_list(Doc)}).


