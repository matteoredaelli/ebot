%%%-------------------------------------------------------------------
%%% File    : ebot_test.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_test).

-export([test/0]).

test() ->
    test_oss().

test_oss() ->
    % U = <<"http://www.redaelli.org/">>,
    ebot_memcache:add_new_url( <<"http://github.com/">> ), 
    ebot_memcache:add_new_url( <<"http://code.google.com/">>),
    ebot_memcache:add_new_url( <<"http://www.gitorious.org/">> ),
    ebot_memcache:add_new_url( <<"http://www.sourceforge.net/">> ),
    ebot_memcache:add_new_url( <<"http://www.freshmeat.net/">> ),
    ebot_memcache:add_new_url( <<"http://www.ohloh.net/">> ),

    ebot_web:start_crawlers().
