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
%%% File    : ebot_html_analyzer_header.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  28 Dec 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_html_analyzer_header).

%% API
-export([add_header_tags/2]).

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

add_header_tags(Url, Body) ->
    {ok, BodyTags} = ebot_util:get_env(tobe_saved_html_tags_data),
    Tokens = mochiweb_html:tokens(Body),
    DeepList = lists:map(
		 fun(TagName) ->
			 analyze_html_tag(Url, Tokens, TagName)
		 end,
		 BodyTags
		),
    lists:flatten(DeepList).

%%====================================================================
%% Internal functions
%%====================================================================


analyze_html_tag(Url, Tokens, <<"title">>) ->
    analyze_html_tag_data(Url, Tokens, <<"title">>);	    
analyze_html_tag(Url, _Tokens, TagName) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_html_tag, Url, {unexpected_tag, TagName}}}).

analyze_html_tag_data(Url, Tokens, TagName) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_html_tag_data, Url, TagName}}),
    case List = ebot_html_util:get_start_tags_data(Tokens, TagName) of
	[] -> 
	    Values = <<"nodata">>;
	List ->
	     Values = ebot_util:bjoin(List)
    end,	
    [{update_field_key_value, TagName, Values}].


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
