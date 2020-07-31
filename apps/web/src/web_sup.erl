%%%-------------------------------------------------------------------
%% @doc web top level supervisor.
%% Ref Supervisor spec docs:
%%   https://erlang.org/doc/man/supervisor.html#supervision-principles
%% @end
%%%-------------------------------------------------------------------

-module(web_sup).

-behaviour(supervisor).

-export([start_link/0, add_worker/1]).

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
    intensity => 0,
    period => 1},

  ChildSpecs = [#{
    id => web_reporter,
    start => {web_reporter, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [web_reporter, gen_server]
  }],

  {ok, {SupFlags, ChildSpecs}}.

add_worker(Url) ->
  supervisor:start_child(?SERVER, #{
    id => Url,
    start => {web_worker, start_link, [Url]},
    restart => temporary,
    shutdown => 2000,
    type => worker,
    modules => [web_worker, gen_server]
  }).


%% internal functions
