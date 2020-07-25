%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2020 10:59 AM
%%%-------------------------------------------------------------------
-module(web).
-author("Aaron Lelevier").
-vsn(1.0).
-export([fetch_and_save/1, create_request/1, create_request/2,
  fetch_page/1, findall/2, findsingle/2]).
-include_lib("dta/include/macros.hrl").
-include_lib("web/include/records.hrl").


%% @doc generic fetch page and save to html/json
fetch_and_save(Req = #request{}) ->
  % impl by "web_file" and dynamic per the "Req" value
  {ok, Body} = web:fetch_page(Req#request.url),
  ok = web_file:file_write(Req, Body),
  Map = web_file:product_map(Req),
  ok = web_file:file_write_product_map(Req, Map),
  ok.


-spec create_request(web_request:url()) -> #request{}.
create_request(Url) ->
  create_request(Url, [{dt, dateutil:date_str()}]).

%% Use to create request for use a specific Dt(datetime) string
-spec create_request(web_request:url(), Opts) -> #request{} when
  Opts :: [{dt, web_request:dt()}].
create_request(Url, Opts) ->
  #request{
    url = Url,
    % optional arguments
    dt = proplists:get_value(dt, Opts, dateutil:date_str()),
    % computed properties
    brand = web_url:brand(Url),
    bike = web_url:bike(Url),
    product_map_target = proplists:get_value(product_map_target, Opts)
  }.


%% @doc fetch a HTML page and caches it to the "priv/html" dir
%% returns binary() of page contents
-spec fetch_page(Url :: string()) -> {ok, Bytes} when
  Bytes :: [byte()].
fetch_page(Url) ->
  % fetch page
  ?LOG({url, Url}),
  ReqHeaders = [{"User-Agent", "dta"}],
  {ok, {_Status, _Headers, Body}} = httpc:request(get, {Url, ReqHeaders}, [], []),
  {ok, Body}.


%% credit: [jaerlang2](https://pragprog.com/titles/jaerlang2/#resources)
findall(Path, Tree) ->
  L1 = findall(Tree, lists:reverse(Path), [], []),
  lists:reverse(L1).

findall({Tag, A, C}, [Tag | Path], Path, L) ->
  [{A, C} | L];
findall({Tag, _, C}, Want, Path, L) ->
  findall(C, Want, [Tag | Path], L);
findall([H | T], Want, Path, L) ->
  L1 = findall(H, Want, Path, L),
  findall(T, Want, Path, L1);
findall(_, _, _, L) ->
  L.


%% @doc Find a single HTML DOM Element's contents
findsingle(Tree, Target) ->
  findsingle(Tree, Target, []).

findsingle({_A, B, C}, Target, L) ->
  case lists:member(Target, B) of
    true -> C;
    false -> findsingle(C, Target, L)
  end;
findsingle([H | T], Target, L) ->
  L1 = findsingle(H, Target, L),
  findsingle(T, Target, L1);
findsingle(_, _, L) ->
  L.
