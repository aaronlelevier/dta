%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).
-compile(export_all).
-include_lib("web/include/records.hrl").

-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, dateutil:date_str()).

create_request(Url, Dt) ->
  #request{
    url = Url,
    dt = Dt,
    brand = web_url:brand(Url),
    bike = web_url:bike(Url)
  }.

filename(Req = #request{}) ->
  filename:join(
    dirname(Req),
    string:concat(Req#request.bike, ".html")
  ).

dirname(Req = #request{}) ->
  filename:join([
    code:priv_dir(web),
    Req#request.brand,
    Req#request.dt
  ]).

