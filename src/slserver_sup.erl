-module(slserver_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {#{strategy => one_for_all},
        [#{id => lobby_mgr, start => {lobby_mgr, start_link, []}},
         #{id => games_sup,
           start => {games_sup, start_link, []},
           type => supervisor}]}}.
