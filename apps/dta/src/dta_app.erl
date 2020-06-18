%%%-------------------------------------------------------------------
%% @doc dta public API
%% @end
%%%-------------------------------------------------------------------

-module(dta_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dta_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
