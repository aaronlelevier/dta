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
-export([extract_content/1, findall/2, findsingle/2]).

%%
-spec extract_content(list()) -> binary().
extract_content(L) ->
  [H | _] = L,
  {_A2, B2} = H,
  [Bin | _] = B2,
  Bin.

%% credit: [jaerlang2](https://pragprog.com/titles/jaerlang2/#resources)
findall(Path, Tree) ->
  L1 = findall(Tree, lists:reverse(Path), [], []),
  lists:reverse(L1).

findall({Tag, A, C}, [Tag | Path], Path, L) ->
  [{A, C} | L];
findall({Tag, _, C}, Want, Path, L) ->
  findall(C, Want, [Tag | Path], L);
findall([H | T], Want, Path, L) ->
  L1 = findall(H, Want, Path, L),
  findall(T, Want, Path, L1);
findall(_, _, _, L) ->
  L.

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
