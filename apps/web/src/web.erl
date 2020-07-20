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
-include_lib("web/include/records.hrl").
-behavior(web_request).

%% DEBUG
-compile(export_all).


-spec create_request(web_request:url()) -> #request{}.
create_request(Url) -> #request{url = Url}.

%% web_request impl interface: start

filename(Req) ->
  html_filename(Req#request.url).

dirname(_) ->
  erlang:error(not_implemented).

file_read(_) ->
  erlang:error(not_implemented).

file_write(_, _) ->
  erlang:error(not_implemented).

%% web_request impl interface: stop


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
  file:write_file(html_filename(Url), Body).


-spec read_file(Url :: string) -> {ok, binary()}.
read_file(Url) ->
  File = html_filename(Url),
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
