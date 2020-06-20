%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Module for all things email related
%%%
%%% @end
%%% Created : 18. Jun 2020 1:09 PM
%%%-------------------------------------------------------------------
-module(email).
-author("Aaron Lelevier").
-vsn(1.0).
-export([send_email/2]).


%% @doc Sends an email async
-spec send_email(Subject::string(), Message::string()) -> {ok, pid()}.
send_email(Subject, Message) ->
  FromEmail = os:getenv("DTA_TEST_EMAIL_USERNAME"),
  ToEmail = os:getenv("DTA_TEST_TO_EMAIL_USERNAME"),
  gen_smtp_client:send({FromEmail, [ToEmail],
    util:str_format(
      "Subject: ~s\r\nFrom: DTA Test\r\nTo: ~p\r\n\r\n~s",
      [Subject, FromEmail, ToEmail, Message])},
    [
      {relay, "smtp.gmail.com"},
      {username, FromEmail},
      {password, os:getenv("DTA_TEST_EMAIL_PW")},
      {auth, always},
      {ssl, true}
    ]
  ).
