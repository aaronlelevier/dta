%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 9:42 AM
%%%-------------------------------------------------------------------
-module(dateutil_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


now_timestamp_str_test() ->
  {Yr, Mo, Day, Hr} = {2020, 7, 4, 9},
  ?assertEqual("2020-07-04:0900", dateutil:now_timestamp_str(Yr, Mo, Day, Hr)).


date_str_test() ->
  {Yr, Mo, Day} = {2020, 8, 11},
  ?assertEqual("2020-08-11", dateutil:date_str(Yr, Mo, Day)).
