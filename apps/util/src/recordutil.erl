%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2020 8:54 AM
%%%-------------------------------------------------------------------
-module(recordutil).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").
-export([record_to_proplist/1, proplist_to_record/2]).

%% @doc Converts a record to a proplist
%% Ref: https://stackoverflow.com/a/3762102/1913888
record_to_proplist(#variant{} = Rec) ->
  lists:zip(record_info(fields, variant), tl(tuple_to_list(Rec)));
record_to_proplist(#inventory_diff{} = Rec) ->
  lists:zip(record_info(fields, inventory_diff), tl(tuple_to_list(Rec))).

%% @doc Converts a proplist to a record
%% Ref: https://stackoverflow.com/a/873666/1913888
proplist_to_record(Proplist, variant_inventory_diff) ->
 list_to_tuple(
   [variant_inventory_diff|[proplists:get_value(X, Proplist) ||
     X <- record_info(fields, variant_inventory_diff)]]).
