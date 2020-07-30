%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(web_worker).
-behaviour(gen_server).

-include_lib("dta/include/macros.hrl").

%% API
-export([start_link/0, send_report/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

send_report(Pid, {Url, BikeMod}) ->
  ?LOG({self(), send_report, start}),
  % TODO: change to 'cast' ?
  gen_server:call(Pid, {send_report, {Url, BikeMod}}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #{}}.

handle_call({send_report, {Url, BikeMod}}, _From, State) ->
  ?LOG({self(), report_sent, Url, BikeMod}),
  ok = web_reporter:receive_report({Url, BikeMod}),
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
