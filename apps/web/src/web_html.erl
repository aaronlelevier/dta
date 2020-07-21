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
-export([extract_content/1, findsingle/2]).

%%
-spec extract_content(list()) -> binary().
extract_content(L) ->
  [H | _] = L,
  {_A2, B2} = H,
  [Bin | _] = B2,
  Bin.


%% @doc Find a single HTML DOM Element's contents
findsingle(Tree, Target) ->
  findsingle(Tree, Target, []).

findsingle({_A, B, C}, Target, L) ->
  case lists:member(Target, B) of
    true -> C;
    false -> findsingle(C, Target, L)
  end;
findsingle([H | T], Target, L) ->
  L1 = findsingle(H, Target, L),
  findsingle(T, Target, L1);
findsingle(_, _, L) ->
  L.
