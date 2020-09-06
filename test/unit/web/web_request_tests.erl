%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2020 11:19 AM
%%%-------------------------------------------------------------------
-module(web_request_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

create_request_test() ->
  Url = eunit_helpers:url(),

  Ret = web_request:create_request(Url),

  ?assertEqual(request, element(1, Ret)).

bike_mod_from_a_web_request_test() ->
  Ret = web_request:bike_mod(eunit_helpers:req()),
  ?assertEqual(chromag, Ret).

bike_mod_from_a_brand_name_test() ->
  Ret = web_request:bike_mod("chromagbikes"),
  ?assertEqual(chromag, Ret).
