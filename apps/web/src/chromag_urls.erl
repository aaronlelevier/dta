%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 8:19 AM
%%%-------------------------------------------------------------------
-module(chromag_urls).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, href_blocks/1, extract_hrefs/1, format_urls/1]).

-define(USE_CACHE, true).

%% @doc queries the home page and returns a list of all bike Urls we care about
-spec urls() -> [dta_types:url()].
urls() ->
  case ?USE_CACHE of
    true -> cached_urls();
    false -> fetch_urls()
  end .

-spec fetch_urls() -> [dta_types:url()].
fetch_urls() ->
  {ok, Body} = web:fetch_page("https://chromagbikes.com/"),
  Tree = mochiweb_html:parse(list_to_binary(Body)),

  % navbar element tree
  NavTree = web_html:findsingle(
    Tree, {<<"class">>, <<"HorizontalList HorizontalList--spacingExtraLoose">>}),

  [DivsA, DivsB, DivsC | _] = web_html:findall(
    [<<"li">>, <<"div">>, <<"div">>, <<"div">>], NavTree),

  HrefBlocks = lists:flatten([
    href_blocks(DivsA),
    href_blocks(DivsB),
    href_blocks(DivsC)
  ]),

  Urls = extract_hrefs(HrefBlocks),

  format_urls(Urls).

-spec cached_urls() -> [dta_types:url()].
cached_urls() ->
  ["https://chromagbikes.com/collections/29-27/products/frames-arcturian",
    "https://chromagbikes.com/collections/29-27/products/frames-doctahawk",
    "https://chromagbikes.com/collections/29-27/products/frames-primer",
    "https://chromagbikes.com/collections/29-27/products/frames-rootdown-20",
    "https://chromagbikes.com/collections/29-27/products/frames-surface",
    "https://chromagbikes.com/collections/29-27/products/frames-surface-ti-19",
    "https://chromagbikes.com/collections/27-5-26/products/frames-sam65",
    "https://chromagbikes.com/collections/27-5-26/products/stylus-2020",
    "https://chromagbikes.com/collections/27-5-26/products/frames-wideangle-19",
    "https://chromagbikes.com/collections/26/products/frames-monk-20"].


%% @doc finds all HrefBlocks from the Divs
-spec href_blocks(list()) -> list().
href_blocks(Divs) ->
  web_html:findall([<<"ul">>, <<"li">>, <<"a">>], element(2, Divs)).


%% @doc extracts hrefs from their surrounding data blocks
-spec extract_hrefs(list()) -> list().
extract_hrefs(HrefBlocks) ->
  extract_hrefs(HrefBlocks, []).

extract_hrefs([], Acc) -> lists:reverse(Acc);
extract_hrefs([H | T], Acc) ->
  {[{_, Href} | _], _} = H,
  extract_hrefs(T, [Href | Acc]).


%% @doc formats a list of Urls to be all deep links
%% urls are initially a combination of deep links and relative links
-spec format_urls(Urls) -> [dta_types:url()] when
  Urls :: [binary()].
format_urls(Urls) ->
  format_urls(Urls, []).

format_urls([], Acc) -> lists:reverse(Acc);
format_urls([H|T], Acc) ->
  Url = case re:run(H, <<"^/collections[.]*">>, [{capture, all, list}]) of
    nomatch -> binary_to_list(H);
    {match, _} -> string:concat("https://chromagbikes.com", binary_to_list(H))
  end,
  format_urls(T, [Url|Acc]).