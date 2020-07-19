%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 8:06 AM
%%%-------------------------------------------------------------------
-module(chromag2).
-author("Aaron Lelevier").
-vsn(1.0).
-export([fetch_and_save/1]).


-spec fetch_and_save(web_request:url()) -> ok.
fetch_and_save(Url) ->
  {ok, Body} = web:fetch_page(Url),
  Req = raaw:create_request(
    Url, [{product_info_location, [<<"html">>, <<"body">>, <<"div">>, <<"main">>, <<"div">>, <<"section">>,
      <<"div">>, <<"div">>, <<"div">>, <<"div">>, <<"script">>]}]),
  % save raw file
  ok = raaw:file_write(Req, Body),
  % QUESTION: could save the product JSON per day or convert later?
  Map = raaw:product_map(Req),
  ok = file:write_file(
    raaw:filename(Req, [{extension, "json"}]),
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.