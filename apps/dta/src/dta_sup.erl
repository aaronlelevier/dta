%%%-------------------------------------------------------------------
%% @doc dta top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dta_sup).

-behaviour(supervisor).

-export([start_link/0, start_worker/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 10,
    period => 1},

  ChildSpecs = [#{
    id => dta_reporter,
    start => {dta_reporter, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [dta_reporter, gen_server]
  }],

  {ok, {SupFlags, ChildSpecs}}.

start_worker(Url) ->
  supervisor:start_child(?SERVER, #{
    id => Url,
    start => {dta_worker, start_link, [Url]},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modules => [dta_worker, gen_server]
  }).
