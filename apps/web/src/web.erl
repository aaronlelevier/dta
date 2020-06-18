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
-export([]).
-compile(export_all).


%% @doc fetch a HTML page and caches it to the "priv/html" dir
%% returns binary() of page contents
-spec fetch_page(Url :: string()) -> {ok, binary()}.
fetch_page(Url) ->
  % fetch page
  {ok, {_Status, _Headers, Body}} = httpc:request(Url),
  % generate abs path and filename
  File = html_filename(Url),
  % write to file
  ok = file:write_file(File, Body),
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
  Ret = case re:run(Bin, <<"will be online from [0-9a-zA-Z\s]+\.">>) of
          {match, _} -> false; % product not posted yet
          nomatch -> true
        end,
  Ret.
