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
%%% File    : ebot_html_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  3 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_html_util).

%% API
-export([
	 get_links/2,
	 get_tag_value/2
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get_links 
%% Description:
%%--------------------------------------------------------------------

get_links(Html, ParentUrl) when is_binary(ParentUrl) ->
    Links = get_links(Html, binary_to_list(ParentUrl)),
    lists:map( fun list_to_binary/1, Links);

get_links(Html, ParentUrl) ->
    Tokens = mochiweb_html:tokens(Html),

    List = lists:foldl(
	     fun(Token, Links) -> 
		     case Token of 
			 {start_tag,<<"a">>,[{<<"href">>,Url}],false} ->
			     case ebot_url_util:is_valid_link(Url) of
				 true ->
				     AbsoluteUrl = ebot_url_util:convert_to_absolute_url( 
						     binary_to_list(Url), 
						     ParentUrl
						    ),
				     [AbsoluteUrl|Links];
				 false ->
				     Links
			     end;
			 _Else ->
			     Links
		     end
	     end,
	     [], 
	     Tokens
	    ),
    ebot_util:remove_duplicates(List).

%% works for <<"title">>
get_tag_value(Html, TagName) ->
    Tag =  {start_tag,TagName,[],false},
    case find_tag(Html, Tag) of
	[Tag,  {data, Title, false} | _Tokens] ->
	    Title;
	_Else ->
	    <<"">>
    end.
    
%%====================================================================
%% Internal functions
%%====================================================================

find_tag(Html, Tag) ->
    Tokens = mochiweb_html:tokens(Html),
    lists:dropwhile(fun(E) -> E =/= Tag end, Tokens).
