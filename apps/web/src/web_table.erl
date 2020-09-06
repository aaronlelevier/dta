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
-include_lib("dta/include/records.hrl").
-include_lib("dta/include/macros.hrl").
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
    <h1>~s inventory changes</h1>
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
    ", [BikeMod, tables(BikeMod)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tables(BikeMod) ->
  Reqs = [web:create_request(Url) || Url <- BikeMod:urls()],
  string:join([table(Req) || Req <- Reqs, table(Req) =/= []], "<br>").

table(Req = #request{}) ->
  Body = tbody(Req),
  if
    Body == [] -> "";
    true -> format("<table>~s~s</table>", [thead(), Body])
  end.

thead() ->
  Ths = string:join([format("<th>~s</th>", [X]) || X <- record_info(fields, variant_inventory_diff)], ""),
  format("<thead><tr>~s<t/tr></thead>", [Ths]).

tbody(Req = #request{}) ->
  Diffs = chromag_variant_inventory:diffs(Req),
  Rows = tds(Diffs),
  if
    Rows == [] -> "";
    true -> format("<tbody>~s</tbody>", [Rows])
  end.

tds(Diffs) ->
  string:join(
    [format("<tr>~s</tr>", [td(Diff)]) || Diff <- Diffs,
      Diff#variant_inventory_diff.quantity /= Diff#variant_inventory_diff.prev_quantity], ""
  ).

td(Diff) ->
  string:join([format("<td>~s</td>", [
    if is_integer(X) =:= true -> integer_to_list(X);
      true -> X
    end
  ]) || X <- tuple_to_list(Diff),
    % we don't want the first field which is the record identifier
    X /= variant_inventory_diff], "").
