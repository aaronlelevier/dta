%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:36 AM
%%%-------------------------------------------------------------------
-module(raaw).
-behavior(web_request).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  create_request/1, create_request/2, filename/1, dirname/1, file_read/1, file_write/2,
  product_map/1]).
-include_lib("web/include/records.hrl").

-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, dateutil:date_str()).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(web_request:url(), string()) -> #request{}.
create_request(Url, Dt) ->
  #request{
    url = Url,
    dt = Dt,
    brand = web_url:brand(Url),
    bike = web_url:bike(Url)
  }.

-spec filename(#request{}) -> string().
filename(Req = #request{}) ->
  filename:join(
    dirname(Req),
    string:concat(Req#request.bike, ".html")
  ).

-spec dirname(#request{}) -> string().
dirname(Req = #request{}) ->
  filename:join([
    code:priv_dir(web),
    Req#request.brand,
    Req#request.dt
  ]).

-spec file_write(#request{}, [byte()]) -> ok | {error, string()}.
file_write(Req = #request{}, Bytes) ->
  ok = fileutil:make_dir(dirname(Req)),
  ok = file:write_file(filename(Req), Bytes),
  ok.

-spec file_read(#request{}) -> {ok, binary()} | {error, string()}.
file_read(Req = #request{}) ->
  file:read_file(raaw:filename(Req)).

-spec product_map(#request{}) -> map().
product_map(Req = #request{}) ->
  {ok, Bin} = file_read(Req),
  Tree = mochiweb_html:parse(Bin),
  L = web:findall([<<"html">>, <<"body">>, <<"div">>, <<"div">>, <<"script">>], Tree),
  Bin2 = web_html:extract_content(L),
  jsx:decode(Bin2).
