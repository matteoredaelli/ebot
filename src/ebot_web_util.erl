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
	 fetch_url_get/1,
	 fetch_url_head/1,
	 fetch_url_links/1
	]).

%%--------------------------------------------------------------------
%% Functions:  
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------

fetch_url_get(URL) ->
    fetch_url(URL, get).

fetch_url_head(URL) ->
    fetch_url(URL, head).

fetch_url_links(URL) ->
    case fetch_url(URL, get) of
	{error, Reason} ->
	    {error, Reason};
	{ok, {_Status, _Headers, Body}} -> 
	    Links = ebot_html_util:get_links(Body, URL),
	    {ok, Links}
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

fetch_url(Url, Command) when is_binary(Url) ->
    fetch_url(binary_to_list(Url), Command);
fetch_url(Url, Command) ->
    {ok, Http_header} = ebot_util:get_env(web_http_header),
    {ok, Request_options} = ebot_util:get_env(web_request_options),
    {ok, Http_options} = ebot_util:get_env(web_http_options),
    try 
	case http:request(Command, {Url,Http_header},Http_options,[{sync, false}|Request_options]) of
	    {ok, RequestId} ->
		receive 
		    {http, {RequestId, Result}} -> 
			case Result of 
			    {error, AReason} ->
				error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, AReason}}),
				Result;
			    Result ->
				{ok, Result}
			end
		after ?EBOT_WEB_TIMEOUT
		      -> 
			error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, timeout}}),
			{error, timeout} 
		end;
	    Error ->
		error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, Error}}),
		Error
	end
    catch
	Reason -> 
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, cannot_fetch_url, Reason}}),
	    {error, Reason}
    end.
