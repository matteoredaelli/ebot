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

%% @doc TEMPLATE.

-module(ebot).
-author('author <matteo.redaelli@libero.it>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the ebot server.
start() ->
    ebot_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
%    ensure_started(couchbeam),
    application:start(ebot).

%% @spec stop() -> ok
%% @doc Stop the ebot server.
stop() ->
    Res = application:stop(ebot),
    application:stop(webmachine),
%    application:stop(couchbeam),
    application:stop(crypto),
    Res.
