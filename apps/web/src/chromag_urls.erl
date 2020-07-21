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


%% @doc queries the home page and returns a list of all bike Urls we care about
-spec urls() -> [web_request:url()].
urls() ->
  {ok, Body} = web:fetch_page("https://chromagbikes.com/"),
  Tree = mochiweb_html:parse(list_to_binary(Body)),

  % navbar element tree
  NavTree = web_html:findsingle(
    Tree, {<<"class">>, <<"HorizontalList HorizontalList--spacingExtraLoose">>}),

  [DivsA, DivsB, DivsC | _] = web:findall(
    [<<"li">>, <<"div">>, <<"div">>, <<"div">>], NavTree),

  HrefBlocks = lists:flatten([
    href_blocks(DivsA),
    href_blocks(DivsB),
    href_blocks(DivsC)
  ]),

  Urls = extract_hrefs(HrefBlocks),

  format_urls(Urls).


%% @doc finds all HrefBlocks from the Divs
-spec href_blocks(list()) -> list().
href_blocks(Divs) ->
  web:findall([<<"ul">>, <<"li">>, <<"a">>], element(2, Divs)).


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
-spec format_urls(Urls) -> [web_request:url()] when
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