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
%%% File    : ebot_html_analyzer_header.erl.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  28 Dec 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_html_analyzer_images).

%% API
-export([add_images_list/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

add_images_list(Url, Tokens) ->
    %% List = ebot_html_util:get_images_from_tokens(Tokens, Url),
    %% TODO
    List = [],
    %% Save Image urls to db and add them to the queue of urls to be visited
    %% lists:foreach(fun ebot_db:open_or_create_url/1, List), 
    %% lists:foreach(fun ebot_crawler:add_url/1, List), 
    [{update_value, <<"images">>, list_to_binary(List)}].

%%====================================================================
%% Internal functions
%%====================================================================
