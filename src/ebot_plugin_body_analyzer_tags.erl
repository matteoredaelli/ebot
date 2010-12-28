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
%%% File    : ebot_plugin_body_analyzer_tags.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  28 Dec 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_plugin_body_analyzer_tags).

%% API
-export([analyze_url_body/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

analyze_url_body(Url, Body) ->
    {ok, BodyTags} = ebot_util:get_env(tobe_saved_body_tags),
    lists:map(
      fun(Tag) ->
	      error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_tags, Url, Tag}}),
	      Value = get_tag_value(Body, Tag),
	      {update_field_key_value, Tag, Value}
      end,
      BodyTags
     ).

%%====================================================================
%% Internal functions
%%====================================================================

%% works for <<"title">>
get_tag_value(Html, TagName) ->
    Tag =  {start_tag,TagName,[],false},
    case find_tag(Html, Tag) of
	[Tag,  {data, Title, false} | _Tokens] ->
	    Title;
	_Else ->
	    <<"">>
    end.

find_tag(Html, Tag) ->
    Tokens = mochiweb_html:tokens(Html),
    lists:dropwhile(fun(E) -> E =/= Tag end, Tokens).
