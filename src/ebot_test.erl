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
%%% File    : ebot_test.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_test).

-compile(export_all).


test() ->
    Mods = [ebot_url_util, ebot_mq, ebot_db, ebot_web, ebot_web_util, ebot_worker_util],
    lists:foreach(
      fun(M) -> M:test() end,
      Mods).

gitorious() ->
    Urls = [ <<"http://www.gitorious.org/">> ],
    test_workers_with_urls(Urls).

google() ->
    Urls = [ <<"http://code.google.com/">> ],
    test_workers_with_urls(Urls).

tyre() ->
    Urls = [ <<"http://www.tyres-pneus-online.co.uk/">> ],
    test_workers_with_urls(Urls).

oss() ->
    Urls = [
     <<"http://github.com/">>, 
     <<"http://www.apache.org/">>,
     <<"http://code.google.com/">>,
     <<"http://www.gitorious.org/">>,
     <<"http://www.sourceforge.net/">>,
     <<"http://www.freshmeat.net/">>,
     <<"http://www.ohloh.net/">>,
     <<"http://raa.ruby-lang.org/">>,
     <<"http://pypi.python.org/pypi">>,
     <<"https://launchpad.net/">>],
    test_workers_with_urls(Urls).

test_workers_with_urls(Urls) ->
    ebot_db:delete_all_docs(),
    timer:sleep(5),
    ebot_crawler:start_workers(),
    lists:foreach( fun ebot_crawler:add_new_url/1, Urls).
