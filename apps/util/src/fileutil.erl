%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc file utility functions
%%%
%%% @end
%%% Created : 04. Jul 2020 8:11 AM
%%%-------------------------------------------------------------------
-module(fileutil).
-author("Aaron Lelevier").
-vsn(1.0).
-export([make_dir/1]).


%% @doc Creates a directory, if it exists already, that's a success
-spec make_dir(Dir::string()) -> ok | {error, string()}.
make_dir(Dir) ->
  % create or confirm the parent Dir exists (works recursively)
  ok = filelib:ensure_dir(Dir),
  % create or confirm Dir exists
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok;
    {error, Reason} -> {error, Reason}
  end.
