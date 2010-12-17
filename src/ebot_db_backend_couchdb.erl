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
-module(ebot_db_backend_couchdb).

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
    case couchbeam:open_doc(Db, Url) of
	{error, _} ->
	    ok;
	{ok, {Doc}} ->
	    couchbeam:delete_doc(Db, Doc)
    end.

empty_db_urls(_Db) ->
    {not_yet_implemented_for, ?MODULE}.

list_urls(_Db) ->
    {not_yet_implemented_for, ?MODULE}.

open_url(Db, Url) ->
    open_doc(Db, Url).

save_url_doc(Db, Url, Doc) ->
    case dict:find(<<"_id">>, Doc) of
	error ->
	    NewDoc =  dict:store(<<"_id">>, Url, Doc);
	_ ->
	    NewDoc = Doc
    end,
    save_doc(Db, NewDoc).

statistics(Db) ->
    Doc = couchbeam:db_info(Db),
    DiskSize = round(proplists:get_value(<<"disk_size">>, Doc) / 1024 / 1024),
    DocCount = proplists:get_value(<<"doc_count">>, Doc),
    [{<<"disk_size">>, DiskSize}, {<<"doc_count">>,DocCount}].

%%====================================================================
%% Internal Functions
%%====================================================================

open_doc(Db, Id) ->
    case couchbeam:open_doc(Db, Id) of
	{ok, {Doc}} ->
	    dict:from_list(Doc);
	{error, Reason} ->
	    Reason
    end.

save_doc(Db, Doc) ->
    {ok, {Doc1}} = couchbeam:save_doc(Db, {dict:to_list(Doc)}),
    dict:from_list(Doc1).


