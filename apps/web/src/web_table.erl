%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Constructs HTML Tables
%%% Ref:
%%%   CSS: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
%%% @end
%%% Created : 19. Aug 2020 7:02 AM
%%%-------------------------------------------------------------------
-module(web_table).
-author("Aaron Lelevier").
-vsn(1.0).
-include_lib("web/include/records.hrl").
-import(stringutil, [format/2]).
-export([html/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Constructs the full HTML with all Bike Inventory Diffs where inventory counts changed
-spec html(BikeMod) -> string() when
  % raaw - not yet supported because of 'product_map' JSON structure
  BikeMod :: chromag.
html(BikeMod) ->
  format(
    "<html>
    <head>
      <style>
        table,
          td {
              border: 1px solid #333;
          }
          thead,
          tfoot {
              background-color: #333;
              color: #fff;
          }
      </style>
    </head>
    <body>~s</body>
    </html>
    ", [tables(BikeMod)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tables(BikeMod) ->
  string:join([table(web:create_request(Url)) || Url <- BikeMod:urls()], "<br>").

table(Req = #request{}) ->
  format("<table>~s~s</table>", [thead(), tbody(Req)]).

thead() ->
  Ths = string:join([format("<th>~s</th>", [X]) || X <- record_info(fields, variant_inventory_diff)], ""),
  format("<thead><tr>~s<t/tr></thead>", [Ths]).

tbody(Req = #request{}) ->
  Diffs = chromag_variant_inventory:diffs(Req),
  format("<tbody>~s</tbody>", [tds(Diffs)]).

tds(Diffs) ->
  string:join(
    [format("<tr>~s</tr>", [td(Diff)]) || Diff <- Diffs], ""
  ).

td(Diff) ->
  % we don't want the first field which is the record identifier
  string:join([format("<td>~s</td>", [
    if is_integer(X) =:= true -> integer_to_list(X);
      true -> X
    end
  ]) || X <- tuple_to_list(Diff),
    X /= variant_inventory_diff,
    Diff#variant_inventory_diff.quantity /= Diff#variant_inventory_diff.prev_quantity], "").
