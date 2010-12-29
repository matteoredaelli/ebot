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
	 get_start_tags_data/2
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
    Urls = get_start_tags_attributes(Tokens, <<"a">>, <<"href">>),
    List = lists:foldl(
	     fun(Url, Links) -> 
		     case ebot_url_util:is_valid_link(Url) of
			 true ->
			     AbsoluteUrl = ebot_url_util:convert_to_absolute_url( 
					     binary_to_list(Url), 
					     ParentUrl
					    ),
			     [AbsoluteUrl|Links];
			 false ->
			     Links
		     end
	     end,
	     [], 
	     Urls
	    ),
    ebot_util:remove_duplicates(List).
    
%%====================================================================
%% Internal functions
%%====================================================================

%% [{<<"href">>, url1},{<<"img">>, file}]
%% there should be only one attribute with that name, but 
%% to be suse, will be returned a list
 
get_attribute_value_list(Attributes, Attribute) ->
    lists:foldl(
      fun(Elem, Results) -> 
	      case Elem of 
		  {Attribute, Value} ->
		      [Value|Results];
		  _Else ->
		      Results
	      end
      end,
      [], 
      Attributes
     ).

get_start_tags_indexes(Tokens, TagName) ->
    lists:foldl(
      fun(Index, Results) -> 
	      Token = lists:nth(Index, Tokens),
	      case Token of 
		  {start_tag,TagName,_,_} ->
		      [Index|Results];
		  _Else ->
		      Results
	      end
      end,
      [], 
      lists:seq(1, length(Tokens))
     ).

get_start_tags_attributes(Tokens, TagName) ->
    Indexes = get_start_tags_indexes(Tokens, TagName),
    lists:map(
      fun(Index) ->
	      {start_tag, TagName, Values, _} = lists:nth(Index, Tokens),
	      Values
      end,
      Indexes).

get_start_tags_attributes(Tokens, TagName, Attribute) ->
    AllTagsAttributes = get_start_tags_attributes(Tokens, TagName), 
    DeepList = lists:foldl(
		 fun(TagAttributes, Results) ->
	      NewValues =  get_attribute_value_list(TagAttributes, Attribute),
			 case NewValues of 
			     [] ->
				 Results;
			     _Else ->
				 [NewValues|Results]
			 end
		 end,
		 [], 
		 AllTagsAttributes
		),
    lists:flatten(DeepList).

get_start_tags_data(Tokens, TagName) ->
    Indexes = get_start_tags_indexes(Tokens, TagName),
    lists:foldl(
      fun(Index, Results) ->
	      case lists:nth(Index + 1, Tokens) of
		  {data, Data, _Whitespace} ->
		      [Data|Results];
		  _Else ->
		      error_logger:warning_report({?MODULE, ?LINE, {get_start_tags_data, data_token_notFound}}),
		      Results
	      end
      end,
      [],
      Indexes).
