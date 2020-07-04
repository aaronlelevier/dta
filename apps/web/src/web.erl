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
-export([fetch_page/1, html_filename/1, is_available/1, write_html_to_file/2,
  fetch_page_and_write_to_file/1, read_file/1]).
-include_lib("dta/include/macros.hrl").

%% DEBUG
-compile(export_all).


%% @doc fetch a HTML page and caches it to the "priv/html" dir
%% returns binary() of page contents
-spec fetch_page(Url :: string()) -> {ok, binary()}.
fetch_page(Url) ->
  % fetch page
  ?LOG({url, Url}),
  ReqHeaders = [{"User-Agent", "dta"}],
  {ok, {_Status, _Headers, Body}} = httpc:request(get, {Url, ReqHeaders}, [], []),
  {ok, Body}.


%% @doc if you want to fetch the HTML contents and write them to a file
-spec fetch_page_and_write_to_file(Url :: string()) -> ok.
fetch_page_and_write_to_file(Url) ->
  {ok, Body} = fetch_page(Url),
  ok = write_html_to_file(Url, Body),
  ok.


-spec write_html_to_file(string(), binary()) -> ok.
write_html_to_file(Url, Body) ->
  file:write_file(web:html_filename(Url), Body).


-spec read_file(Url :: string) -> {ok, binary()}.
read_file(Url) ->
  File = web:html_filename(Url),
  file:read_file(File).


%% @doc returns the abspath of the filename based on the Url
-spec html_filename(Url :: string()) -> string().
html_filename(Url) ->
  Page = page_name(Url),
  PrivDir = code:priv_dir(?MODULE),
  filename:join([PrivDir, "html", string:concat(Page, ".html")]).


%% @doc Returns the Url page name
-spec page_name(Url :: string()) -> string().
page_name(Url) ->
  [_ | [Page | _]] = string:split(Url, "/", trailing),
  Page.


%% @doc Returns the page name with the html suffix
-spec html_page_name(Str :: string()) -> string().
html_page_name(Str) ->
  string:concat(Str, ".html").


%% @doc returns a boolean if the product is available
-spec is_available(Url :: string()) -> boolean().
is_available(Url) ->
  {ok, Bin} = fetch_page(Url),
  is_available_using_contents(Bin).


-spec is_available_using_contents(binary()) -> boolean().
is_available_using_contents(Bin) ->
  case re:run(Bin, <<"will be online from [0-9a-zA-Z\s]+\.">>) of
    {match, _} -> false; % product not posted yet
    nomatch -> true
  end.


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