%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 19. Aug 2020 7:06 AM
%%%-------------------------------------------------------------------
-module(stringutil).
-author("Aaron Lelevier").
-vsn(1.0).
-export([format/2]).

%% @doc Performs string interpolation
-spec format(string(), [any()]) -> string().
format(String, Args) ->
  lists:flatten(io_lib:format(String, Args)).
