%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc mochiweb HTML helpers
%%%
%%% @end
%%% Created : 13. Jul 2020 7:40 AM
%%%-------------------------------------------------------------------
-module(web_html).
-author("Aaron Lelevier").
-vsn(1.0).
-export([extract_content/1]).

%%
-spec extract_content(list()) -> binary().
extract_content(L) ->
  [H | _] = L,
  {_A2, B2} = H,
  [Bin | _] = B2,
  Bin.
