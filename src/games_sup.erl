-module(games_sup).
-behaviour(supervisor).

%% API.
-export([new_coop_game/0]).

%% supervisor
-export([start_link/0]).
-export([init/1]).

% internal
-export([new_game/1]).

%% API.
-spec new_coop_game() -> {ok, pid()}.
new_coop_game() ->
  supervisor:start_child(?MODULE, [coop]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {#{strategy => simple_one_for_one},
        [#{id => game,
           start => {?MODULE, new_game, []},
           restart => temporary}]}}.

new_game(coop) ->
  coop_game:start_link().
