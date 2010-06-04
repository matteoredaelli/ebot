%%%-------------------------------------------------------------------
%%% File    : ebot_plugin_body_analyzer_sample.erl
%%% Author  : matteo <matteo@pirelli.com>
%%% Description : 
%%%
%%% Created :  4 Jun 2010 by matteo <matteo.redaelli@libero.it>
%%%-------------------------------------------------------------------
-module(ebot_plugin_body_analyzer_sample).

%% API
-export([analyze_url_body/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

analyze_url_body(_Body, _Url) ->
    [
     {update_field_key_value, <<"sample_key">>, 1}
     %% {update_field_timestamp,<<"ebot_timestamp_key">>},
     %% {update_field_counter, <<"ebot_counter_key">>},
    ].

%%====================================================================
%% Internal functions
%%====================================================================
