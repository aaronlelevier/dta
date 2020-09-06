%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Entrypoint to scrape a single page.
%%%
%%% @end
%%% Created : 20. Jun 2020 3:59 PM
%%%-------------------------------------------------------------------
-module(scrape).
-author("Aaron Lelevier").
-vsn(1.0).
-export([main/1]).
-include_lib("dta/include/macros.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main([string()]) -> ok.
main(Args) ->
  io:format("Version: ~p~n", [init:script_id()]),
  io:format("Args: ~p~n", [Args]),

  % start applications
  ok = start(),

  % TODO: hardcoded 'BikeMod' until other bike mods are supported
  ok = dta_reporter:send_work(chromag),

  ok = email:send_email("Chromag Inventory", web_table:html(chromag)),

  erlang:halt(0),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% start DETS
start() ->
  ok = ssl:start(),
  ok = inets:start(),
  ok = application:start(dta),
  ok.
