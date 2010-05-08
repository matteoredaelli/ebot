%%%-------------------------------------------------------------------
%%% File    : ebot_test.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  2 May 2010 by matteo <matteo@pirelli.com>
%%%-------------------------------------------------------------------
-module(ebot_test).

-export([test1/0]).

test1() ->
    % U = <<"http://www.redaelli.org/">>,
    U = <<"http://github.com/">>,
    ebot_memcache:add_new_url(U), 
    ebot_web:start_crawlers().
