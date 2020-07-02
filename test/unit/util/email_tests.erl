%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2020 7:08 AM
%%%-------------------------------------------------------------------
-module(email_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

assert_sent_success_test() ->
  Bin = <<"2.0.0 OK  1593697148 i196sm9203199pgc.55 - gsmtp\r\n">>,
  ?assertEqual({ok, email_sent}, email:assert_sent(Bin)).

assert_sent_failed_test() ->
  Bin = <<"4.0.3 OK  1593697148 i196sm9203199pgc.55 - gsmtp\r\n">>,
  ?assertEqual({error, email_not_sent}, email:assert_sent(Bin)).
