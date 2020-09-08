%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2020 7:05 AM
%%%-------------------------------------------------------------------
-module(raaw_variant_inventory).
-author("Aaron Lelevier").
-vsn(1.0).
-export([]).
-compile(export_all).
-include_lib("dta/include/records.hrl").


-spec diffs(#request{}) -> [#variant_inventory_diff{}].
diffs(Req) -> [Req].