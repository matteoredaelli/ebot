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
%% Function: analyze_url_bod
%% Description: All values of tags found in the option "tobe_saved_body_tags" 
%% of file sys.config will be saved to db
%%
%% supported tags are: title
%%      {tobe_saved_body_tags, 
%%	  [
%%	   <<"title">>
%%	  ]},
%%--------------------------------------------------------------------

analyze_url_body(Url, Body) ->
    {ok, BodyTags} = ebot_util:get_env(tobe_saved_body_tags),
    Tokens = mochiweb_html:tokens(Body),
    lists:map(
      fun(TagName) ->
	      error_logger:info_report({?MODULE, ?LINE, {analyze_url_body_tags, Url, TagName}}),
	      {_, Value} = get_tag_value(Tokens, TagName),
	      {update_field_key_value, TagName, Value}
      end,
      BodyTags
     ).

%%====================================================================
%% Internal functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DESCRITION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% {start_tag,<<"meta">>,
%%            [{<<"name">>,<<"description">>},
%%             {<<"content">>,<<"Website dei Fratelli Redaell"...>>}],
%%            true},

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KEYWORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {start_tag,<<"meta">>,
%%            [{<<"name">>,<<"keywords">>},
%%             {<<"content">>,<<"redaelli,carate brianza,opensource,l"...>>}],\
%%            true},
%% {data,<<"\n">>,true},



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IMG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {start_tag,<<"img">>,
%%            [{<<"src">>,<<"images/matteo.jpg">>}],
%%            true},
%% {start_tag,<<"img">>,
%%            [{<<"class">>,<<"image">>},
%%             {<<"src">>,
%%              <<"http://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Carate_bria"...>>},
%%             {<<"width">>,<<"120">>},
%%             {<<"alt">>,<<"Icon">>}],
%%            true}]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TITLE (<<"title">>)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tag_value(Tokens, TagName) ->
    case find_start_tag(Tokens, TagName) of
	[_,  {data, Value, false} | _OtherTokens] ->
	    {ok, Value};
	_Else ->
	    {error, <<"">>}
    end.

find_start_tag(Tokens, TagName) ->
    lists:dropwhile(fun(E) -> 
			    case E of
				{start_tag, TagName, _, _} ->
				    false;
				_Else ->
				    true
			    end
		    end, 
		    Tokens).

%%find_tag(Tokens, Tag) ->
%%    lists:dropwhile(fun(E) -> E =/= Tag end, Tokens).
