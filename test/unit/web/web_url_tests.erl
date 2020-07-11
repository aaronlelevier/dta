%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 8:57 AM
%%%-------------------------------------------------------------------
-module(web_url_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

%% Helpers
raaw_url() ->
  "https://raawmtb.com/collections/frames-bikes/products/madonna-fox-factory-build".

chromag_url() ->
  "https://chromagbikes.com/collections/27-5-26/products/stylus-2020".

%% Tests
brand_test() ->
  ?assertEqual("raawmtb", web_url:brand(raaw_url())),
  ?assertEqual("chromagbikes", web_url:brand(chromag_url())).

bike_test() ->
  ?assertEqual("madonna-fox-factory-build", web_url:bike(raaw_url())),
  ?assertEqual("stylus-2020", web_url:bike(chromag_url())).
