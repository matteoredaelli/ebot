%%%-------------------------------------------------------------------
%%% File    : ebot_test.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_test).

-export([
	 test/0,
	 test1/0,
	 test_oss/0
	]).

test() ->
    Mods = [ebot_url_util, ebot_mq, ebot_db, ebot_web],
    lists:foreach(
      fun(M) -> M:test() end,
      Mods).

test1() ->
    Urls = [ <<"http://www.gitorious.org/">> ],
    test_crawlers_with_urls(Urls).

test_oss() ->
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
    test_crawlers_with_urls(Urls).

test_crawlers_with_urls(Urls) ->
    ebot_db:empty_db_urls(),
    timer:sleep(5),
    lists:foreach( fun ebot_cache:add_new_url/1, Urls),
    ebot_web:start_crawlers().
