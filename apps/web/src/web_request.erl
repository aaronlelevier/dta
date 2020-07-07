%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2020 7:06 AM
%%%-------------------------------------------------------------------
-module(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").

-export([read_file/2]).
-export_type([url/0, dt/0]).

%% Types
-type url() :: string().

%% datetime string of format: 2020-07-04:0900 i.e. YYYY-MM-DD:hhmm
-type dt() :: string().

%% Behaviors
-callback filename(#request{}) -> string().

%% Public API

-spec read_file(#request{}, function()) -> {ok, binary()} | {error, Reason} when
  Reason :: string().
read_file(Req, Fun) -> file:read_file(Fun(Req)).