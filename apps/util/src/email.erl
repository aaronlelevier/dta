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
-export([send_email/2, assert_sent/1]).
-include_lib("dta/include/macros.hrl").

%% @doc Sends and email and blocks until complete
%% success is asserted, and will raise exception of email send fails
-spec send_email(Subject :: string(), Message :: string()) -> ok.
send_email(Subject, Message) ->
  FromEmail = os:getenv("DTA_TEST_EMAIL_USERNAME"),
  ToEmail = os:getenv("DTA_TEST_TO_EMAIL_USERNAME"),
  Pw = os:getenv("DTA_TEST_EMAIL_PW"),
  ?LOG({from_email, FromEmail}),
  ?LOG({to_email, ToEmail}),
  ?LOG({pw_loaded, is_list(Pw)}),
  Bin = gen_smtp_client:send_blocking({FromEmail, [ToEmail],
    util:str_format(
      "Subject: ~s\r\nFrom: DTA Test\r\nTo: ~p\r\n\r\n~s",
      [Subject, FromEmail, Message])},
    [
      {relay, "smtp.gmail.com"},
      {username, FromEmail},
      {password, Pw},
      {auth, always},
      {ssl, true}
    ]
  ),
  % will fail (and should fail) here on a pattern match if email
  % send fails
  {ok, email_sent} = assert_sent(Bin),
  ok.

assert_sent(Bin) ->
  {match, ReRet} = re:run(Bin, <<"^([\.0-9]+?)\s.*">>),
  [{Start, Length} | _] = tl(ReRet),
  IsSuccess = string:slice(Bin, Start, Length) =:= <<"2.0.0">>,
  if
    IsSuccess == true ->
      ?LOG({success, email_sent, os:getenv("DTA_TEST_TO_EMAIL_USERNAME")}),
      {ok, email_sent};
    true ->
      ?LOG({fail, email_not_sent, os:getenv("DTA_TEST_TO_EMAIL_USERNAME")}),
      {error, email_not_sent}
  end.
