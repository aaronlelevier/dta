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

%% Should be called with a single positional argument, which is the BikeMod
%% ex: chromag
-spec main([string()]) -> ok.
main(Args) ->
  io:format("Version: ~p~n", [init:script_id()]),
  io:format("Args: ~p~n", [Args]),

  [H|_] = Args,
  BikeMod = list_to_atom(H),

  % start applications
  ok = start(),

  ok = dta_reporter:send_work(BikeMod),
  ok = email:send_email(
    stringutil:format("~s inventory", [BikeMod]),
    web_table:html(BikeMod)),

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
