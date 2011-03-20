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
%%% File    : ebot_html_analyzer_sample.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Jun 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_html_analyzer_sample).

%% API
-export([analyze_url_body/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

analyze_url_body(_Url, _Tokens) ->
    %% add here you logic
    %% 
    [
     {update_value, <<"sample_key">>, 1}
     %% {update_timestamp,<<"ebot_timestamp_key">>},
     %% {update_counter, <<"ebot_counter_key">>},
    ].

%%====================================================================
%% Internal functions
%%====================================================================
