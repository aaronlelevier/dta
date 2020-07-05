%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Datetime utility functions
%%%
%%% @end
%%% Created : 04. Jul 2020 7:26 AM
%%%-------------------------------------------------------------------
-module(dateutil).
-author("Aaron Lelevier").
-vsn(1.0).
-export([now_timestamp_str/0, now_timestamp_str/4]).


%% @doc Returns a datetime string of the local time in format: "2020-07-04:0700"
%% precision is up to the hour
-spec now_timestamp_str() -> string().
now_timestamp_str() ->
  {{Yr, Mo, Day}, {Hr, _Min, _Sec}} = calendar:local_time(),
  io_lib:format('~b-~2..0b-~2..0b:~2..0b00', [Yr, Mo, Day, Hr]).


%% @doc Use to construct the timestamp from explicit arguments
now_timestamp_str(Yr, Mo, Day, Hr) ->
  io_lib:format('~b-~2..0b-~2..0b:~2..0b00', [Yr, Mo, Day, Hr]).
