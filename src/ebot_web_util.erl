%%%-------------------------------------------------------------------
%%% File    : ebot_web_util.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_web_util).
-author("matteo.redaelli@libero.it").


-include("ebot.hrl").

%% API
-export([
	 fetch_url/5,
	 fetch_url_get/4,
	 fetch_url_head/4,
	 fetch_url_links/4
	]).

%%--------------------------------------------------------------------
%% Functions:  
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------

fetch_url_get(URL, Http_header, Http_options, Request_options) ->
    fetch_url(URL, get, Http_header, Http_options, Request_options).

fetch_url_head(URL, Http_header, Http_options, Request_options) ->
    fetch_url(URL, head, Http_header, Http_options, Request_options).

fetch_url_links(URL, Http_header, Http_options, Request_options) ->
    case fetch_url_get(URL, Http_header, Http_options, Request_options) of
	{error, Reason} ->
	    {error, Reason};
	{ok, {_Status, _Headers, Body}} -> 
	    Links = ebot_html_util:get_links(Body, URL),
	    {ok, Links}
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

fetch_url(URL, Command, Http_header, Http_options, Request_options) when is_binary(URL) ->
    fetch_url(binary_to_list(URL), Command, Http_header, Http_options, Request_options);
fetch_url(URL, Command, Http_header, Http_options, Request_options) ->
    case http:request(Command, {URL,Http_header},Http_options,[{sync, false}|Request_options]) of
	{ok, RequestId} ->
	    receive 
		{http, {RequestId, Result}} -> 
		    case Result of 
			{error, _} ->
			    Result;
			Result ->
			    {ok, Result}
		    end
	    after ?EBOT_WEB_TIMEOUT
		      -> {error, timeout} 
	    end;
	Error ->
	    Error
    end.
