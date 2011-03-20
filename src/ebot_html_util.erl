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
	 get_links_from_tokens/2,
	 get_images/2,
	 get_images_from_tokens/2,
	 get_start_tags_attributes/2,
	 get_start_tags_attributes/3,
	 get_start_tags_data/2
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get_links_from_tokens 
%% Description:
%%--------------------------------------------------------------------

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

get_images(Html, ParentUrl) ->
    Tokens = mochiweb_html:tokens(Html),
    get_images_from_tokens(Tokens, ParentUrl).

get_images_from_tokens(Tokens, ParentUrl) when is_binary(ParentUrl) ->
    Images = get_images_from_tokens(Tokens, binary_to_list(ParentUrl)),
    lists:map( fun list_to_binary/1, Images);
get_images_from_tokens(Tokens, ParentUrl) ->
    Urls = lists:flatten(get_start_tags_attributes(Tokens, <<"img">>, <<"src">>)),
    List = lists:foldl(
	     fun(Url, Links) -> 
		     case ebot_url_util:is_valid_image(Url) of
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

get_links(Html, ParentUrl) ->
    Tokens = mochiweb_html:tokens(Html),
    get_links_from_tokens(Tokens, ParentUrl).

get_links_from_tokens(Tokens, ParentUrl) when is_binary(ParentUrl) ->
    Links = get_links_from_tokens(Tokens, binary_to_list(ParentUrl)),
    lists:map( fun list_to_binary/1, Links);
get_links_from_tokens(Tokens, ParentUrl) ->
    Urls = lists:flatten(get_start_tags_attributes(Tokens, <<"a">>, <<"href">>)),
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
    DeepList = lists:foldl(
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
		 Indexes),
    lists:flatten(DeepList).


%%====================================================================
%% EUNIT TESTS
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_html_util_test() ->
    Url = <<"http://www.redaelli.org/">>,
    Html = <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"> 
<html xmlns=\"http://www.w3.org/1999/xhtml\"> 
<head> 
<title>Fratelli Redaelli</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /> 
<title>Redaelli.Org</title> 
<meta name=\"keywords\" content=\"redaelli,carate brianza,opensource,linux,italy\" /> 
<meta name=\"description\" content=\"Website dei Fratelli Redaelli\" /> 
<link href=\"templatemo_style.css\" rel=\"stylesheet\" type=\"text/css\" /> 
<meta name=\"google-site-verification\" content=\"SdYRvDUubCbqDXBUbur7qnC1Gh9cmVC3GUisrpqGBT0\" /> 
</head> 
<body> 
<img src=\"images/fratelli-redaelli.jpg\" /> 
<img class=\"image\" src=\"http://upload.wikimedia.org/test.JPG\" width=\"120\" alt=\"Icon\" /> 
                    <ul class=\"templatemo_list bullet_arrow\"> 
                    	<li><a href=\"http://www.alpinicarate.it\">Alpini Carate</a></li> 
						<li><a href=\"matteo/\">Matteo</a></li> 
						<li><a href=\"http://www.vvfcarate.it/\">Vigili del Fuoco</a></li> 
                    </ul> 
</body> 
</html>">>,
    Tokens = mochiweb_html:tokens(Html),

    ?assertEqual( [<<"http://www.alpinicarate.it">>,
		   <<"http://www.redaelli.org/matteo">>,
		   <<"http://www.vvfcarate.it/">>],
		   get_links_from_tokens(Tokens, Url)),

    ?assertEqual( [<<"http://upload.wikimedia.org/test.JPG">>,
		   <<"http://www.redaelli.org/images/fratelli-redaelli.jpg">>],
		  get_images_from_tokens(Tokens, Url)),

    ?assertEqual( [[{<<"name">>,<<"google-site-verification">>},
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
    		  get_start_tags_attributes(Tokens, <<"meta">>)).

-endif.
