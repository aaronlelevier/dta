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
-export([now_timestamp_str/0, now_timestamp_str/4, date_str/0, date_str/3]).
-export_type([date_string/0]).

%% @doc Date string of format "YYYY-MM-DD"
-type date_string() :: string().


%% @doc Returns a datetime string of the local time in format: "2020-07-04:0700"
%% precision is up to the hour
-spec now_timestamp_str() -> string().
now_timestamp_str() ->
  {{Yr, Mo, Day}, {Hr, _Min, _Sec}} = calendar:local_time(),
  now_timestamp_str(Yr, Mo, Day, Hr).


%% @doc Use to construct the timestamp from explicit arguments
now_timestamp_str(Yr, Mo, Day, Hr) ->
  io_lib:format('~b-~2..0b-~2..0b:~2..0b00', [Yr, Mo, Day, Hr]).


%% @doc Returns date string in format: "2020-07-04"
-spec date_str() -> date_string().
date_str() ->
  {{Yr, Mo, Day}, {_Hr, _Min, _Sec}} = calendar:local_time(),
  date_str(Yr, Mo, Day).


%% @doc Use to construct the timestamp from explicit arguments
-spec date_str(integer(), integer(), integer()) -> date_string().
date_str(Yr, Mo, Day) ->
  io_lib:format('~b-~2..0b-~2..0b', [Yr, Mo, Day]).
