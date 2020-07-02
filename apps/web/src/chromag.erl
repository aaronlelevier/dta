%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Calls web module functions on the Chromag HTML structure
%%%
%%% @end
%%% Created : 02. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(chromag).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).

%% DEBUG
-compile(export_all).


%% @doc destructure the HTML Tree to the JSON binary contents that we care about
-spec raw_json_data(Tree::tuple()) -> binary().
raw_json_data(Tree) ->
  L = web:findall([<<"html">>, <<"body">>, <<"div">>, <<"main">>, <<"div">>, <<"section">>,
  <<"div">>, <<"div">>, <<"div">>, <<"div">>, <<"script">>], Tree),
  [H|_] = L,
  {_A2,B2} = H,
  [Bin|_] = B2,
  Bin.


%% @doc load HTML tree from a local file based on the Url
-spec html_tree(Url::string()) -> tuple().
html_tree(Url) ->
  {ok, Bin} = web:read_file(Url),
  Tree = mochiweb_html:parse(Bin),
  Tree.


%% NEXT: destructure the map
%% @doc Returns the Product JSON as an Erlang map
-spec product_map() -> map().
product_map() ->
  Url = "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
  Tree = html_tree(Url),
  Bin = raw_json_data(Tree),
  jsx:decode(Bin).