%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Erlang Module helpers
%%%
%%% @end
%%% Created : 07. Aug 2020 8:57 AM
%%%-------------------------------------------------------------------
-module(moduleutil).
-author("Aaron Lelevier").
-vsn(1.0).
-export([show_exports/1]).

%% @doc helper for outputting all exportable functions
%% this is useful if starting out with: -compile(export_all)
%% example call:
%% $ moduleutil:show_exports(foo).
%% $ -export([foo/1])
%% $ ok.
-spec show_exports(atom()) -> ok.
show_exports(Mod) ->
  Exports = [
    lists:flatten(io_lib:format("~p/~p", [X, Y])) ||
    {X, Y} <- Mod:module_info(exports),
    X /= module_info],
  io:format(
    "-export([~s]).~n", [string:join(Exports, ", ")]
  ).
