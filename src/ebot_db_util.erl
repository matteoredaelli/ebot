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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	 create_doc/3,
	 delete_doc/2,
	 list_docs/1,
	 open_doc/2,
	 open_or_create_db/0,
	 open_or_create_doc/3,
	 save_doc/3,
	 update_doc/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

create_doc(Db, ID, DocType) when is_atom(ID) ->
    create_doc(Db, list_to_binary(atom_to_list(ID)), DocType);
create_doc(Db, ID, DocType) when is_list(ID) ->
    create_doc(Db, list_to_binary(ID), DocType);
create_doc(Db, ID, DocType) ->
    {ok, Doc} = ebot_db_doc:create_doc(ID, DocType),
    save_doc(Db, ID, Doc).

delete_doc(Db, ID) ->
    ?EBOT_DB_BACKEND:delete_doc(Db, ID).

list_docs(Db) ->
    ?EBOT_DB_BACKEND:list_docs(Db).

open_doc(Db, ID) when is_atom(ID) ->
    open_doc(Db, list_to_binary(atom_to_list(ID)));
open_doc(Db, ID) when is_list(ID) ->
    open_doc(Db, list_to_binary(ID));
open_doc(Db, ID) ->
    ?EBOT_DB_BACKEND:open_doc(Db, ID).


open_or_create_db() ->
    {ok, Hostname} = ebot_util:get_env(db_hostname),
    {ok, Port} = ebot_util:get_env(db_port),
    case ?EBOT_DB_BACKEND of
	ebot_db_backend_couchdb ->
	    application:start(ibrowse),
	    application:start(couchbeam),
	    Prefix = "",
	    Options = [],
	    Conn = couchbeam:server_connection(Hostname, Port, Prefix, Options),
	    case couchbeam:server_info(Conn) of
		{ok, _Version} ->
		    couchbeam:open_or_create_db(Conn, "ebot", []);
		Else ->
		    error_logger:error_report({?MODULE, ?LINE, {init, cannot_connect_to_db, ?EBOT_DB_BACKEND, Else}}),
		    Else
	    end;
	ebot_db_backend_riak_pb ->
	    riakc_pb_socket:start_link(Hostname, Port);
	_Else ->
	    error_logger:error_report({?MODULE, ?LINE, {init, unsupported_backend, ?EBOT_DB_BACKEND}}),
	    {error, unsupported_backend}
    end.

open_or_create_doc(Db, ID, DocType) ->
    case open_doc(Db, ID) of
	{error, not_found} ->
	    error_logger:warning_report({?MODULE, ?LINE, {open_or_create_doc, ID, doesnt_exist, will_be_created}}),
	    create_doc(Db, ID, DocType);
	{ok, Doc} ->
	    {ok, Doc}
    end.

save_doc(Db, ID, Doc) when is_list(ID) ->
    save_doc(Db, list_to_binary(ID), Doc);
save_doc(Db, ID, Doc) ->
    ?EBOT_DB_BACKEND:save_doc(Db, ID, Doc),
    {ok, Doc}.

update_doc(Db, ID, Options) ->
    case open_doc(Db, ID) of
	{ok, Doc} ->
	    {ok, Doc2} = ebot_db_doc:update_doc(Doc, Options),
	    save_doc(Db, ID, Doc2);
	{error,Reason} ->
	    {error,Reason}
    end.


%%====================================================================
%% EUNIT TESTS
%%====================================================================

-ifdef(TEST).

ebot_db_util_test() ->
    {ok, DB} = open_or_create_db(),
    ID = <<"http://www.redaelli.org/matteo/ebot_test/">>,
    Key = <<"ebot_referrals">>,
    open_or_create_doc(DB, ID, url),
    {ok, Doc} = open_doc(DB, ID),
    ?assertEqual({ok,0}, dict:find(Key, Doc)),
    update_doc(DB, ID, [{update_value, Key, <<>>}]),
    {ok, Doc2} = open_doc(DB, Doc),
    ?assertEqual({ok,1}, dict:find(Key, Doc2)),
    update_doc(DB, ID, [
			{update_value, <<"ebot_head_error">>, <<"">> },
			{update_value, <<"content_length">>, <<"12345">> },
			{update_timestamp,<<"ebot_head_visited">>},
			{update_counter,<<"ebot_visits_count">>},
			{update_value,<<"ebot_errors_count">>,1},
			{update_value, <<"content-type">>, <<"text/html; charset=UTF-8">>}
		       ]),
    {ok, Doc3} = open_doc(DB, ID),
    ?assertEqual({ok,<<"">>}, dict:find(<<"ebot_head_error">>, Doc3)),
    ?assertEqual({ok,<<"12345">>}, dict:find(<<"content_length">>, Doc3)),
    ?assertEqual({ok,1}, dict:find(<<"ebot_errors_count">>, Doc3)),
    update_doc(DB, ID, [
			{head,
			 {{"HTTP/1.1",200,"OK"},
			  [{"cache-control",
			    "private, max-age=0, must-revalidate"},
			   {"connection","keep-alive"},
			   {"date","Wed, 22 Sep 2010 19:12:39 GMT"},
			   {"via","1.1 varnish"},
			   {"age","0"},
			   {"etag",
			    "\"9de4ba5a2cf11a66a3543fd2c07ae683\""},
			   {"server","Apache"},
			   {"vary","Accept-Encoding"},
			   {"content-length","11521"},
			   {"content-type","text/html; charset=utf-8"},
			   {"x-powered-by",
			    "Phusion Passenger (mod_rails/mod_rack) 2.2.11"},
			   {"x-runtime","20"},
			   {"set-cookie",
			    "_gitorious_sess=8b512c1b5212a4b330ee2e8a1673153e; domain=.gitorious.org; path=/; expires=Wed, 13 Oct 2010 19:12:39 GMT; HttpOnly"},
			   {"status","200"},
			   {"x-varnish","942297164"}
			  ],
			  empty
			 },
			 [<<"content-length">>,<<"content-type">>,
			  <<"server">>,<<"x-powered-by">>, <<"UNKNOWNHEADER">>
			 ]
			}
		       ]
	      ),
    
%   What happens if you update a not existing url?
    update_doc(DB, <<"http://code.google.com/">>,
	       [
		{update_counter,<<"ebot_errors_count">>},
		{update_value,<<"ebot_head_error">>, <<"timeout">>}
	       ]
	      ),
    {ok, Doc4} = open_doc(DB, <<"http://code.google.com/">>),
    ?assertEqual({ok,<<"timeout">>}, dict:find(<<"ebot_head_error">>, Doc4)).
    
%%   delete_doc(DB, Url).
%%    ?assertEqual({error,not_found},  ebot_db:open_doc(Url)).

-endif.
