%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(web_reporter).
-behaviour(gen_server).

-include_lib("dta/include/macros.hrl").

%% API
-export([start_link/0, receive_report/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

receive_report({Url, BikeMod}) ->
  ?LOG({self(), receive_report, start}),
  gen_server:call(?SERVER, {receive_report, {Url, BikeMod}}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #{}}.

handle_call({receive_report, {Url, BikeMod}}, _From, State) ->
  ?LOG({self(), report_received, Url, BikeMod}),
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
