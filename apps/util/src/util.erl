%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Utility functions to re-use across modules
%%%
%%% @end
%%% Created : 18. Jun 2020 1:09 PM
%%%-------------------------------------------------------------------
-module(util).
-author("Aaron Lelevier").
-vsn(1.0).
-export([str_format/2]).


%% @doc Takes the same args as "io:format" and returns a string
-spec str_format(String::string(), Args::list()) -> string().
str_format(String, Args) ->
  lists:flatten(io_lib:format(String, Args)).
