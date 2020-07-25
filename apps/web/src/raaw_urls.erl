%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc All Urls we care about for RAAW Madonna
%%%
%%% @end
%%% Created : 19. Jul 2020 7:41 AM
%%%-------------------------------------------------------------------
-module(raaw_urls).
-author("Aaron Lelevier").
-vsn(1.0).
-export([urls/0, madonna_v2_frame_kit/0, madonna_fox_factory_build/0,
  madonna_xtr_build_url/0]).

urls() ->
  [madonna_v2_frame_kit(), madonna_fox_factory_build(), madonna_xtr_build_url()].

madonna_v2_frame_kit() ->
  "https://raawmtb.com/collections/frames-bikes/products/madonna-v2-frame-kit".

madonna_fox_factory_build() ->
  "https://raawmtb.com/collections/frames-bikes/products/madonna-fox-factory-build".

madonna_xtr_build_url() ->
  "https://raawmtb.com/collections/frames-bikes/products/madonna-xtr-build".
