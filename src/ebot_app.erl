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
%% @author author <matteo.redaelli@libero.it>
%% @copyright 2009 author.

%% @doc Callbacks for the ebot application.

-module(ebot_app).
-author('author <matteo.redaelli@libero.it>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ebot.
start(_Type, _StartArgs) ->
    ebot_deps:ensure(),
    ebot_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ebot.
stop(_State) ->
    ok.
