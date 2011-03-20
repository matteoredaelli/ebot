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

add_header_tags(Url, Tokens) ->
    L1 = analyze_html_tag(Url, Tokens, <<"title">>),
    L2 = analyze_html_tag(Url, Tokens, <<"meta">>),
    lists:flatten( [L1, L2]).

%%====================================================================
%% Internal functions
%%====================================================================


analyze_html_tag(Url, Tokens, <<"title">>) ->
    analyze_html_tag_data(Url, Tokens, <<"title">>);	  
analyze_html_tag(Url, Tokens, <<"meta">>) ->
    analyze_html_meta_attributes(Url, Tokens);
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
    [{update_value, TagName, Values}].

analyze_html_meta_attributes(Url, Tokens) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_html_tag_attributes, Url}}),
    Attributes = ebot_html_util:get_start_tags_attributes(Tokens, <<"meta">>),    
    analyze_meta_attributes(Url, Attributes).

analyze_meta_attributes(_Url, []) ->
    [{update_value, <<"keywords">>, <<>>},{update_value, <<"description">>, <<>>}];
analyze_meta_attributes(_Url, Attributes) ->
    lists:foldl(
      fun(Attribute, Results) ->
	      [analyze_meta_attribute(url, Attribute)|Results]
      end,
      [],
      Attributes).

analyze_meta_attribute(_Url, [{_Name,Name},{<<"content">>,Content}]) ->
    %% TODO: Name should be normalized (riak maybe doesn't like some characters like -, upcase chars,.. ok for couchdb)
    {update_value, Name, Content};
analyze_meta_attribute(Url, List) ->
    error_logger:info_report({?MODULE, ?LINE, {analyze_meta_attribute, Url, unexpected_list, List}}),
    {update_value, <<"headermeta">>, <<"invalid_meta-attribute">>}. 

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



%%====================================================================
%% EUNIT TESTS
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

ebot_html_analyzer_header_test() ->
    Url = <<"http://www.redaelli.org/">>,
    MetaAttributes = [[{<<"name">>,<<"google-site-verification">>},
		       {<<"content">>,
			<<"SdYRvDUubCbqDXBUbur7qnC1Gh9cmVC3GUisrpqGBT0">>}],
		      [{<<"name">>,<<"description">>},
		       {<<"content">>,
			<<"Website dei Fratelli Redaelli">>}],
		      [{<<"name">>,<<"keywords">>},
		       {<<"content">>,
			<<"redaelli,carate brianza,opensource,linux,italy">>}],
		      [{<<"http-equiv">>,<<"Content-Type">>},
		       {<<"content">>,
			<<"text/html; charset=utf-8">>}]
		     ],
    ?assertEqual([{update_value,<<"Content-Type">>,
                    <<"text/html; charset=utf-8">>},
		  {update_value,<<"keywords">>,
		   <<"redaelli,carate brianza,opensource,linux,italy">>},
		  {update_value,<<"description">>,
		   <<"Website dei Fratelli Redaelli">>},
		  {update_value,
		   <<"google-site-verification">>,
		   <<"SdYRvDUubCbqDXBUbur7qnC1Gh9cmVC3GUisrpqGBT0">>}],
		 analyze_meta_attributes(Url, MetaAttributes)).
-endif.
