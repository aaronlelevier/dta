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
  FromEmail = os:getenv("DTA_FROM_EMAIL_USERNAME"),
  Pw = os:getenv("DTA_FROM_EMAIL_PW"),
  ToEmail = os:getenv("DTA_TO_EMAIL_USERNAME"),
  ?LOG({from_email, FromEmail}),
  ?LOG({to_email, ToEmail}),
  ?LOG({pw_loaded, is_list(Pw)}),

  Email = {<<"text">>, <<"html">>, [
    {<<"From">>, list_to_binary(FromEmail)},
    {<<"To">>, list_to_binary(ToEmail)},
    {<<"Subject">>, list_to_binary(Subject)}],
    #{content_type_params => [
      {<<"charset">>, <<"US-ASCII">>}],
      disposition => <<"inline">>
    },
    list_to_binary(Message)},
  Encoded = mimemail:encode(Email),

  Bin = gen_smtp_client:send_blocking(
    {FromEmail, [ToEmail], Encoded},
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
      ?LOG({success, email_sent, os:getenv("DTA_TO_EMAIL_USERNAME")}),
      {ok, email_sent};
    true ->
      ?LOG({fail, email_not_sent, os:getenv("DTA_TO_EMAIL_USERNAME")}),
      {error, email_not_sent}
  end.
