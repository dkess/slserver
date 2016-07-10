-module(coop_game).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([join_game/2]).
-export([add_word/3]).
-export([end_words/1]).
-export([attempt_word/2]).
-export([giveup_status/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%-record(state, {
%}).

-record(player, {name, pid, gaveup}).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec join_game(identifier(), string()) -> ok | taken.
join_game(GamePid, Playername) ->
  gen_server:call(GamePid, {join_game, Playername}).

-spec add_word(identifier(), string(), boolean()) -> ok.
add_word(GamePid, Word, IsGuessed) ->
  gen_server:cast(GamePid, {add_word, self(), Word, IsGuessed}).

-spec end_words(identifier()) -> string().
end_words(GamePid) ->
  gen_server:call(GamePid, end_words).

-spec attempt_word(identifier(), string()) -> any().
attempt_word(GamePid, Word) ->
  gen_server:cast(GamePid, {attempt_word, self(), Word}).

-spec giveup_status(identifier(), boolean()) -> any().
giveup_status(GamePid, Gaveup) ->
  gen_server:cast(GamePid, {giveup_status, self(), Gaveup}).

%% gen_server.
init(_) ->
  process_flag(trap_exit, true),
  {ok, just_started}.

handle_call(list_words, _From, State = {playing, WordDict, _PList}) ->
  {reply, dict:fetch_keys(WordDict), State};

handle_call({join_game, Playername}, {FromPid, _Tag}, State) ->
  link(FromPid),
  case State of
    just_started ->
      {reply, {ok, []}, {collecting, dict:new(), FromPid, Playername}};
    {playing, WordDict, PList} ->
      {NewPList, PLState} =
        lists:mapfoldl(
          fun(Player = #player{name=N, pid=Pid}, DidFind) ->
              case {DidFind, N, Pid} of
                {nothing, Playername, quit} ->
                  % this player is replacing someone who quit
                  {Player#player{pid=FromPid}, replaced};
                {nothing, Playername, _} ->
                  % this player name is in use
                  {Player, taken};
                {_, _, _} ->
                  {Player, DidFind}
              end
          end, nothing, PList),

      case PLState of
        taken ->
          {reply, taken, State};
        _ ->
          % send the player_joined message to everyone else
          announce_to_players(fun(Pid) ->
                                  ws_handler:player_joined(Pid, Playername)
                              end, PList),

          NPlist = case PLState of
                     replaced -> NewPList;
                     nothing -> [newplayer(Playername, FromPid) | NewPList]
                   end,

          % inform the newly joined player of the list of players
          % this is reversed because the most recently joined players should
          % be at the end.
          Ps = lists:map(fun(#player{name=N, pid=P}) ->
                             {N, P =/= quit}
                         end, NPlist),
          ws_handler:player_list(FromPid, lists:reverse(Ps)),

          % inform the newly joined player of the list of words
          Ws = dict:fetch_keys(WordDict),
          ws_handler:word_list(FromPid, Ws),

          % inform the newly joined player of the previously guessed words
          dict:fold(fun(Word, Guesser, _) ->
                        case Guesser of
                          noone -> ok;
                          _ -> ws_handler:word_guessed(FromPid, Word, Guesser)
                        end
                    end, ok, WordDict),

          % incorm the newly joined player of who has given up
          lists:map(fun(#player{name=N, gaveup=G}) ->
                        case G of
                          true -> ws_handler:player_giveup(FromPid, N, true);
                          _ -> ok
                        end
                    end, NPlist),

          {reply, ok, {playing, WordDict, NPlist}}
      end
  end;
handle_call(end_words, {From, _Tag}, {collecting, WordDict, From, PName}) ->
  {ok, GameName} = lobby_mgr:register_game(),
  {reply, GameName, {playing, WordDict, [newplayer(PName, From)]}};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({add_word, From, Word, IsGuessed},
            {collecting, WordDict, From, PName}) ->
  Guesser = if IsGuessed -> PName;
               true -> noone
            end,
  {noreply, {collecting, dict:store(Word, Guesser, WordDict), From, PName}};

handle_cast({attempt_word, From, Word}, {playing, WordDict, PList}) ->
  case dict:find(Word, WordDict) of
    {ok, noone} ->
      Guesser = get_playername(PList, From),
      NewDict = dict:store(Word, Guesser, WordDict),

      % inform everyone (except the guesser) that this word has been guessed
      SPList = lists:filter(fun(#player{name=N}) -> N =/= Guesser end, PList),
      announce_to_players(fun(Pid) ->
                              ws_handler:word_guessed(Pid, Word, Guesser)
                          end, SPList),
      {noreply, {playing, NewDict, PList}};
    _ ->
      {noreply, {playing, WordDict, PList}}
  end;

handle_cast({giveup_status, From, Status}, {playing, WordDict, PList}) ->
  % find the player who gave up, and change their flag
  {NewPList, {found, Name}} =
    lists:mapfoldl(fun(P = #player{name=Name, pid=Pid}, PState) ->
                       case {PState, Pid} of
                         {nothing, From} ->
                           {P#player{gaveup=Status}, {found, Name}};
                         _ ->
                           {P, PState}
                       end
                   end, nothing, PList),

    % inform everyone (except the person who sent the message) that this
    % player has given up
    SPList = lists:filter(fun(#player{name=N}) -> N =/= Name end, NewPList),
    announce_to_players(fun(Pid) ->
                            ws_handler:player_giveup(Pid, Name, Status)
                        end, SPList),

    gen_server:cast(self(), check_allgiveup),
    {noreply, {playing, WordDict, NewPList}};

handle_cast(check_allgiveup, State = {playing, WordDict, PList}) ->
  case lists:all(fun(#player{pid=Pid, gaveup=GaveUp}) ->
                     (Pid =:= quit) or GaveUp
                 end, PList) of
    true ->
      announce_to_players(fun ws_handler:allgiveup/1, PList),
      NewWordDict = dict:map(fun(_Word, noone) -> "_";
                                (_Word, Guesser) -> Guesser
                             end, WordDict),
      {noreply, {playing, NewWordDict, PList}};
    _ ->
      {noreply, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', FromPid, Reason}, State = {playing, WordDict, PList}) ->
  MgrPid = whereis(lobby_mgr),
  case FromPid of
    MgrPid ->
      {stop, {mgr_killed, Reason}, State};
    _ ->
      % change this player's state to 'quit' in the player list
      {NewPList, {found, Name}} =
        lists:mapfoldl(fun(P = #player{pid=Pid, name=Name}, PState) ->
                           case {PState, Pid} of
                             {nothing, FromPid} ->
                               {P#player{pid=quit, gaveup=false}, {found, Name}};
                             _ ->
                               {P, PState}
                           end
                       end, nothing, PList),

      % inform everyone else that this player has quit
      announce_to_players(fun(P) -> ws_handler:player_quit(P, Name) end,
                          NewPList),

      gen_server:cast(self(), check_allgiveup),
      {noreply, {playing, WordDict, NewPList}}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_playername([P | Rest], Pid) ->
  case P of
    #player{pid=Pid, name=Name} -> Name;
    _ -> get_playername(Rest, Pid)
  end;
get_playername([], _Pid) ->
  noone.

announce_to_players(Fun, PList) ->
  lists:foldl(fun(#player{pid=Pid}, _) ->
                  case Pid of
                    quit -> ok;
                    _ -> Fun(Pid)
                  end
              end, ok, PList).

newplayer(Name, Pid) ->
  #player{name=Name, pid=Pid, gaveup=false}.
