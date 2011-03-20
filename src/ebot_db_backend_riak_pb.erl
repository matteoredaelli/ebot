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
%%% File    : ebot_db_riak.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  29 Mag 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_db_backend_riak_pb).

-define(BUCKET_URLS, <<"ebot">>).

%% API
-export([
	 delete_doc/2,
	 delete_all_docs/1,
	 list_docs/1,
	 open_doc/2,
	 save_doc/3,
	 statistics/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

delete_doc(Db, Url) ->
    delete_key(Db, ?BUCKET_URLS, Url).

delete_all_docs(Db) ->
    empty_db_bucket(Db, ?BUCKET_URLS).

list_docs(Db) ->
    list_keys(Db, ?BUCKET_URLS).

open_doc(Db, Id) ->
    case riakc_pb_socket:get(Db, ?BUCKET_URLS, Id) of
	{error, notfound} ->
	    {error, not_found};
	{error, Reason} ->
	    error_logger:error_report({?MODULE, ?LINE, {open_doc, error, Reason}}),
	    {error, Reason};
	{ok, Object} -> 
	    {ok, binary_to_term(riakc_obj:get_value(Object))};
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {open_doc, unexpected_value, Else}}),
	    Else
    end.

save_doc(Db, Url, Doc) ->
    save_doc(Db, ?BUCKET_URLS, Url, Doc).

statistics(Db) ->
    %% TODO: disk_size
    {ok, Keys} = list_docs(Db),
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

save_doc(Db, Bucket, Key, Doc) ->
    NewDoc = term_to_binary(Doc, [compressed]),
    case riakc_pb_socket:get(Db, Bucket, Key) of
	{error, notfound} ->
	    NewObject = riakc_obj:new(Bucket, Key, NewDoc);
	{ok, Object} ->
	    NewObject = riakc_obj:update_value(Object, NewDoc)
    end,
    case riakc_pb_socket:put(Db, NewObject) of
	ok ->
	    error_logger:info_report({?MODULE, ?LINE, {save_doc, Key, ok}});
	Else ->
	    error_logger:error_report({?MODULE, ?LINE, {save_doc, Key, error, Else}})
    end,
    {ok, Doc}.



