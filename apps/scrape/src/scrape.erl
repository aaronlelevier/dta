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

-define(APP, dta).

%%====================================================================
%% API functions
%%====================================================================

%% Should be called with a single positional argument, which is the BikeMod
%% ex: chromag
-spec main([string()]) -> ok.
main(Args) ->
  io:format("Version: ~p~n", [init:script_id()]),
  io:format("Args: ~p~n", [Args]),

  % start applications
  application:load(?APP),
  application:load(getopt),

  % parse args and set env params
  Ret = parse_args(Args),
  ?LOG(Ret),

  % start applications
  ok = ssl:start(),
  ok = inets:start(),
  ok = application:start(?APP),

  % retrieve env param
  ?LOG(application:get_all_env(?APP)),

  {ok, BikeMod0} = application:get_env(?APP, bikemod),
  ?LOG(BikeMod0),

  % must be an atom per method contract below
  BikeMod = list_to_atom(BikeMod0),

  ok = dta_reporter:send_work(BikeMod),
  ok = email:send_email(
    stringutil:format("~s inventory", [BikeMod]),
    web_table:html(BikeMod)),

  erlang:halt(0),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

parse_args(Args) ->
  case getopt:parse(opt_spec_list(), Args) of
    {ok, {ParsedArgs, _BadArgs}} ->
      ?LOG(ParsedArgs),
      set_args(ParsedArgs);
    {error, {invalid_option, _}} ->
      getopt:usage(opt_spec_list(), "DTA"),
      erlang:halt(1)
  end.

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
  [{ bikemod
    , $b
    , "bikemod"
    , {string, "raaw"}
    , "Name of BikeMod. Either 'chromag' or 'raaw'"
  }].

set_args([]) -> ok;
set_args([{Arg, Val} | Rest]) ->
  ?LOG(Rest),
  application:set_env(?APP, Arg, Val),
  set_args(Rest).
