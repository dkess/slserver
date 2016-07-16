-module(lobby_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([find_game/1]).
-export([new_coop_game/1]).
-export([register_game/0]).
-export([destroy_game/1]).

-export([validate_name/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%-record(state, {
%}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec find_game(string()) -> {ok, pid()} | noexist.
find_game(GameName) ->
  gen_server:call(?MODULE, {find_game, GameName}).

-spec new_coop_game(string()) -> {ok, pid()} | badname.
new_coop_game(Playername) ->
  case validate_name(Playername) of
    false ->
      badname;
    _ ->
      {ok, GamePid} = gen_server:call(?MODULE, new_coop_game),
      coop_game:join_game(GamePid, Playername),
      {ok, GamePid}
  end.

-spec register_game() -> {ok, string()} | error.
register_game() ->
  gen_server:call(?MODULE, register_game).

-spec destroy_game(string()) -> any().
destroy_game(GameName) ->
  gen_server:cast(?MODULE, {destroy_game, GameName}).

%% gen_server.
init([]) ->
  {ok, dict:new()}.

handle_call(new_coop_game, _From, State) ->
  {reply, games_sup:new_coop_game(), State};

handle_call(register_game, {From, _Tag}, GamesDict) ->
  GameName = generate_gamename(GamesDict),
  NewGamesDict = dict:store(GameName, From, GamesDict),
  {reply, {ok, GameName}, NewGamesDict};

handle_call({find_game, GameName}, _From, GamesDict) ->
  R = case dict:find(GameName, GamesDict) of
        {ok, GamePid} ->
          {ok, GamePid};
        error ->
          noexist
      end,
  {reply, R, GamesDict};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({destroy_game, GameName}, GamesDict) ->
  {noreply, dict:erase(GameName, GamesDict)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% other functions
% Names must be alphanumeric, and between 1-10 chars
-spec validate_name(string()) -> true | false.
validate_name(Name) ->
  re:run(Name, "^[a-zA-Z0-9]{1,10}$") =/= nomatch.

% generate a random game name
-spec generate_gamename(dict:dict()) -> string().
generate_gamename(GamesDict) ->
  Candidate = generate_gamename(7, ""),
  case dict:is_key(Candidate, GamesDict) of
    true ->
      generate_gamename(GamesDict);
    _ ->
      Candidate
  end.

generate_gamename(0, GameName) ->
  GameName;
generate_gamename(N, GameName) ->
  R = rand:uniform(36) - 1,
  C = if R < 26 ->
           $a + R;
         true ->
           $0 + R - 26
      end,
  generate_gamename(N - 1, [C | GameName]).
