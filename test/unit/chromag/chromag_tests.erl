%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2020 8:29 AM
%%%-------------------------------------------------------------------
-module(chromag_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


meck_test() ->
  ok = meck:new(dog, [non_strict]),
  Bark = "Woof!",

  meck:expect(dog, bark, fun() -> Bark end),

  ?assertEqual(Bark, dog:bark()),
  ?assertEqual(true, meck:validate(dog)).
