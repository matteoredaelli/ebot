%%%-------------------------------------------------------------------
%%% File    : ebot_db_riak.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  29 Mag 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_db_backend_riak).

-define(BUCKET_URLS, <<"ebot">>).

%% API
-export([
	 delete_url/2,
	 empty_db_urls/1,
	 list_urls/1,
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

delete_url(Db, Url) ->
    delete_key(Db, ?BUCKET_URLS, Url).

empty_db_urls(Db) ->
    empty_db_bucket(Db, ?BUCKET_URLS).

list_urls(Db) ->
    list_keys(Db, ?BUCKET_URLS).

open_url(Db, Id) ->
    open_doc(Db, ?BUCKET_URLS, Id).

save_url_doc(Db, Url, Doc) ->
    save_doc(Db, ?BUCKET_URLS, Url, Doc).

statistics(Db) ->
    %% TODO: disk_size
    {ok, Keys} = list_urls(Db),
    DocCount = length(Keys),
    [ {<<"disk_size">>, 0}, 
      {<<"doc_count">>,DocCount}
    ].

%%====================================================================
%% Internal Functions
%%====================================================================

delete_key(Db, Bucket, Key) ->
     riakc_pb_socket:delete(Db, Bucket, Key).

empty_db_bucket(Db, Bucket) ->
    case list_keys(Db, ?BUCKET_URLS) of
	{ok, Keys} ->
	    lists:foreach(
	      fun(K) -> delete_key(Db, Bucket, K) end,
	      Keys);
	Else ->
	    Else
    end.

list_keys(Db, Bucket) ->
    riakc_pb_socket:list_keys(Db, Bucket).

open_doc(Db, Bucket, Id) ->
    case riakc_pb_socket:get(Db, Bucket, Id) of
	{error, notfound} ->
	    not_found;
	{error, Reason} ->
	    error_logger:error_report({?MODULE, ?LINE, {open_doc, error, Reason}}),
	    Reason;
	{ok, Object} -> 
	    binary_to_term(riakc_obj:get_value(Object));
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {open_doc, unexpected_value, Else}}),
	    Else
    end.

save_doc(Db, Bucket, Key, Doc) ->
    Object = riakc_obj:new(Bucket, Key, term_to_binary(Doc, [compressed])),
    riakc_pb_socket:put(Db, Object).



