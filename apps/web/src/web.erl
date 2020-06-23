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
-export([fetch_page/1, html_filename/1, is_available/1]).
-include_lib("dta/include/macros.hrl").

%% @doc fetch a HTML page and caches it to the "priv/html" dir
%% returns binary() of page contents
-spec fetch_page(Url :: string()) -> {ok, binary()}.
fetch_page(Url) ->
  % fetch page
  ?LOG({url, Url}),
  {ok, {_Status, _Headers, Body}} = httpc:request(Url),
  {ok, Body}.


%% @doc returns the abspath based on the "Url"
-spec html_filename(Url::string()) -> string().
html_filename(Url) ->
  PrivDir = code:priv_dir(?MODULE),
  [_ | [Page | _]] = string:split(Url, "/", trailing),
  filename:join([PrivDir, "html", string:concat(Page, ".html")]).


%% @doc returns a boolean if the product is available
-spec is_available(Url::string()) -> boolean().
is_available(Url) ->
  {ok, Bin} = fetch_page(Url),
  is_available_using_contents(Bin).


is_available_using_contents(Bin) ->
  case re:run(Bin, <<"will be online from [0-9a-zA-Z\s]+\.">>) of
    {match, _} -> false; % product not posted yet
    nomatch -> true
  end.
