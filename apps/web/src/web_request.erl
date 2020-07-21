%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc "request" record type call back module
%%%
%%% @end
%%% Created : 07. Jul 2020 7:06 AM
%%%-------------------------------------------------------------------
-module(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").
-export_type([url/0, dt/0]).

%% Types
-type url() :: string().

%% datetime string of format: "2020-07-04" i.e. "YYYY-MM-DD"
-type dt() :: string().

%% Behaviors
-callback filename(#request{}) -> string().

-callback dirname(#request{}) -> string().

-callback file_read(#request{}) -> {ok, iodata()} | {error, Reason} when
  Reason :: string().

-callback file_write(#request{}, iodata()) -> ok | {error, Reason} when
  Reason :: string().
