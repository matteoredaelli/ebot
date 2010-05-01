%%%-------------------------------------------------------------------
%%% File    : ebot_html_util.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  3 Apr 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_html_util).

%% API
-export([get_links/2]).

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
			     case ebot_url_util:is_valid_url_using_all_known_invalid_regexps(Url) of
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

%%====================================================================
%% Internal functions
%%====================================================================
