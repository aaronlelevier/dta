%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2020 6:54 AM
%%%-------------------------------------------------------------------
-module(chromag_urls_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").


href_blocks_test() ->
  Divs = {[{<<"class">>, <<"MegaMenu__Item MegaMenu__Item--fit">>}],
    [{<<"a">>,
      [{<<"href">>, <<"/collections/26/frames+current">>},
        {<<"class">>,
          <<"MegaMenu__Title Heading Text--subdued u-h7">>}],
      [<<"26\"">>]},
      {<<"ul">>,
        [{<<"class">>, <<"Linklist">>}],
        [{<<"li">>,
          [{<<"class">>, <<"Linklist__Item">>}],
          [{<<"a">>,
            [{<<"href">>,
              <<"https://chromagbikes.com/collections/26/products/frames-monk-20">>},
              {<<"class">>, <<"Link Link--secondary">>}],
            [<<"Monk">>]}]}]}]},

  Ret = chromag_urls:href_blocks(Divs),

  ?assertEqual(
    [{[{<<"href">>,
      <<"https://chromagbikes.com/collections/26/products/frames-monk-20">>},
      {<<"class">>, <<"Link Link--secondary">>}],
      [<<"Monk">>]}],
    Ret).

extract_hrefs_test() ->
  HrefBlocks = [{[{<<"href">>,
    <<"https://chromagbikes.com/collections/26/products/frames-monk-20">>},
    {<<"class">>, <<"Link Link--secondary">>}],
    [<<"Monk">>]}],

  Ret = chromag_urls:extract_hrefs(HrefBlocks),

  ?assertEqual(
    [<<"https://chromagbikes.com/collections/26/products/frames-monk-20">>],
    Ret).

format_urls_test() ->
  Urls = [
    <<"https://chromagbikes.com/collections/26/products/frames-monk-20">>,
    <<"/collections/27-5-26/products/frames-wideangle-19">>
  ],

  Ret = chromag_urls:format_urls(Urls),

  ?assertEqual(
    ["https://chromagbikes.com/collections/26/products/frames-monk-20",
      "https://chromagbikes.com/collections/27-5-26/products/frames-wideangle-19"],
    Ret).
