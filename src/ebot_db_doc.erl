%% EBOT, an erlang web crawler.
%% Copyright (C) 2010 2011 ~ matteo DOT redaelli AT libero DOT it
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
%%% Created :  2 Jan 2011 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_db_doc).

-include("ebot.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	 create_doc/2,
	 doc_date_field_status/3,
	 doc_get_value/2,
	 update_doc/2,
	 update_doc_by_key_value/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

create_doc(ID, DocType) when is_atom(DocType) ->
    create_doc(ID, list_to_binary(atom_to_list(DocType)));
create_doc(ID, DocType) ->
    Module = get_module(DocType),
    {ok, Doc} = Module:create_doc(ID), 
    update_doc(Doc, [
		     {update_value, <<"ebot_doctype">>, DocType},
		     {update_value, <<"_id">>, ID},
		     {update_timestamp, <<"ebot_created_at">>}
		    ]).

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

get_doctype(Doc) ->
     list_to_atom(binary_to_list(doc_get_value(<<"ebot_doctype">>, Doc))).

update_doc(Doc, [{update_counter, Key}|Options]) ->
    {ok, NewDoc} = update_doc_increase_counter(Doc, Key),
    update_doc(NewDoc, Options);
update_doc(Doc, [{update_timestamp, Key}|Options]) ->
    {ok, NewDoc} = update_doc_timestamp_by_key(Doc, Key),
    update_doc(NewDoc, Options);
update_doc(Doc, [{update_value, Key, Value}|Options]) ->
    {ok, NewDoc} = update_doc_by_key_value(Doc, Key, Value),
    update_doc(NewDoc, Options);
update_doc(Doc, [{add_to_list, Key, Value}|Options]) ->
    List = doc_get_value(Key, Doc),
    case lists:member(Value, List) of
	true ->
	    Doc2 = Doc;
	false ->
	    {ok, Doc2} = update_doc_by_key_value(Doc, Key, [Value|List])
    end,
    update_doc(Doc2, Options);
update_doc(Doc, [{remove_from_list, Key, Value}|Options]) ->
    List = doc_get_value(Key, Doc),
    NewList =  lists:delete(Value, List),
    {ok, Doc2} = update_doc_by_key_value(Doc, Key, NewList),
    update_doc(Doc2, Options);
update_doc(Doc, [Unknown|Options]) ->
    DocType = get_doctype(Doc),
    Module = get_module(DocType),
    error_logger:info_report({?MODULE, ?LINE, update_doc, option, Unknown, sent_to_module, Module}),
    NewDoc = case Module:update_doc(Doc, Unknown) of
		 {ok, Doc2} ->
		     Doc2;
		 _Else ->
		     Doc
	     end,
    update_doc(NewDoc, Options);	
update_doc(Doc, []) ->
    {ok, Doc}.

%%====================================================================
%% Internal functions
%%====================================================================

get_module(DocType) when is_atom(DocType) ->
    get_module(atom_to_list(DocType));
get_module(DocType) when is_binary(DocType) ->
    get_module(binary_to_list(DocType));
get_module(DocType) ->
    list_to_atom("ebot_db_doc_" ++ DocType).

date_field_status(Date, Days) ->
    Now = calendar:datetime_to_gregorian_seconds( calendar:universal_time() ),
    Diff = Now - Date, 
    case Diff > Days * 86400 of
	true ->
	    obsolete;
	false ->
	    updated
    end.

update_doc_increase_counter(Doc, Key) ->
    Value = doc_get_value(Key, Doc),
    update_doc_by_key_value(Doc, Key, Value + 1).

update_doc_timestamp_by_key(Doc, Key) ->
    Value = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    update_doc_by_key_value(Doc, Key, Value).

update_doc_by_key_value(Doc, Key, Value) ->
    %%error_logger:info_report({?MODULE, ?LINE, {update_doc_by_key_value, Key, Value, Doc}}),
    Doc2 = dict:store(Key, Value, Doc),
    {ok, Doc2}.


-ifdef(TEST).

ebot_db_doc_test() ->
    ID = <<"http://www.redaelli.org/">>,
    ID2 =  <<"http://www.redaelli.org/matteo/">>,
    {ok, Doc} = create_doc(ID, url),
    ?assert( url == get_doctype(Doc)),
    ?assert( ebot_db_doc_url == get_module(url)),
    ?assert(12 == length(dict:fetch_keys(Doc))),
    {ok, Doc2} = update_doc(Doc, [{add_to_list, <<"ebot_referrals">>, ID2}]),
    ?assert(12 == length(dict:fetch_keys(Doc2))).
-endif.
