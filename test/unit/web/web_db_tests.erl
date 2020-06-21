%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2020 7:08 AM
%%%-------------------------------------------------------------------
-module(web_db_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


start_test() ->
  TableName = web,
  DetsFile = "web-test.dets",
  Bike = "MetaAm29",

  % table closed
  ?assertEqual(undefined, dets:info(web)),

  % open table
  RetStart = web_db:start(TableName, DetsFile),
  ?assertEqual({ok, web}, RetStart),

  % clear table to maintain a clean test state
  ?assertEqual(ok, dets:delete_all_objects(TableName)),

  % table open
  ?assertEqual(set, dets:info(web, type)),

  % no notifications sent yet, so this will be false
  RetSent = web_db:has_notification_sent(TableName, Bike),
  ?assertEqual(false, RetSent),

  % update table that notification was sent
  RetUpdate = web_db:update_table_notification_sent(TableName, Bike),
  ?assertEqual(ok, RetUpdate),

  % notification sent, so should now return true
  RetSent2 = web_db:has_notification_sent(TableName, Bike),
  ?assertEqual(true, RetSent2),

  % close table
  RetDone = web_db:done(TableName),
  ?assertEqual(ok, RetDone),

  % table closed
  ?assertEqual(undefined, dets:info(web, type)).