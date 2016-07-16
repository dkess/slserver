-module(coop_game).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([watch_game/1]).
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

% The amount of milliseconds a game can be left empty for, before being destroyed
-define(EMPTY_TIME, timer:hours(24)).

% The amount of time in milliseconds it should take to create a game.
-define(CREATE_TIME, 5000).

-record(player, {name :: string(),
                 pid :: pid(),
                 gaveup = false :: boolean()}).

-record(state, {gamename :: string(),
                % mapping of words to their guesser
                wdict :: dict:dict(string(), noone | string()),
                % list of players in order of join time
                plist :: list(#player{}),
                % set of PIDs of websockets that have not yet entered their name
                wset = sets:new() :: sets:set(pid()),
                % Ref to a timer that will end the game after a certain amount
                % of time if no one joins
                destroy_ref = alive :: alive | {timer, timer:tref()}}).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% The message sent when joining a game before having picked a name
-spec watch_game(identifier()) -> any().
watch_game(GamePid) ->
  gen_server:cast(GamePid, {watch_game, self()}).

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
  {ok, Timer} = timer:exit_after(?CREATE_TIME, {timer_end, create_timeout}),
  {ok, {just_started, Timer}}.

handle_call(list_words, _From, State = #state{wdict=WordDict}) ->
  {reply, dict:fetch_keys(WordDict), State};

handle_call({join_game, Playername}, {FromPid, _Tag}, State) ->
  link(FromPid),
  case State of
    {just_started, Timer} ->
      {reply, {ok, []}, {collecting, dict:new(), FromPid, Playername, Timer}};
    #state{wdict=WordDict, plist=PList, wset=WSet, destroy_ref=Destroy} ->
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
          % cancel the destroy timer if there is one
          case Destroy of
            alive -> ok;
            {timer, Timer} ->
              timer:cancel(Timer)
          end,

          NewWSet = sets:del_element(FromPid, WSet),

          % send the player_joined message to everyone else
          announce_to_players(fun(Pid) ->
                                  ws_handler:player_joined(Pid, Playername)
                              end, PList),

          NPlist = case PLState of
                     replaced -> NewPList;
                     nothing ->
                       [#player{name=Playername, pid=FromPid} | NewPList]
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

          {reply, ok, State#state{plist=NPlist,
                                  wset=NewWSet,
                                  destroy_ref=alive}}
      end
  end;

handle_call(end_words, {From, _Tag},
            {collecting, WordDict, From, PName, Timer}) ->
  timer:cancel(Timer),
  process_flag(trap_exit, true),
  {ok, GameName} = lobby_mgr:register_game(),
  {reply,
   GameName,
   #state{gamename=GameName,
          wdict=WordDict,
          plist=[#player{name=PName, pid=From}]}};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({add_word, From, Word, IsGuessed},
            {collecting, WordDict, From, PName, Timer}) ->
  Guesser = if IsGuessed -> PName;
               true -> noone
            end,
  {noreply, {collecting, dict:store(Word, Guesser, WordDict), From, PName, Timer}};

handle_cast({watch_game, Pid},
            State = #state{wset=WSet, destroy_ref=Destroy}) ->
  link(Pid),
  % cancel the destroy timer if there is one
  case Destroy of
    alive -> ok;
    {timer, Timer} ->
      timer:cancel(Timer)
  end,
  {noreply, State#state{wset=sets:add_element(Pid, WSet), destroy_ref=alive}};

handle_cast({attempt_word, From, Word},
            State = #state{wdict=WordDict, plist=PList}) ->
  case dict:find(Word, WordDict) of
    {ok, noone} ->
      Guesser = get_playername(PList, From),
      NewDict = dict:store(Word, Guesser, WordDict),

      % inform everyone (except the guesser) that this word has been guessed
      SPList = lists:filter(fun(#player{name=N}) -> N =/= Guesser end, PList),
      announce_to_players(fun(Pid) ->
                              ws_handler:word_guessed(Pid, Word, Guesser)
                          end, SPList),
      {noreply, State#state{wdict=NewDict}};
    _ ->
      {noreply, State}
  end;

handle_cast({giveup_status, From, Status},
            State = #state{plist=PList}) ->
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
    {noreply, State#state{plist=NewPList}};

handle_cast(check_allgiveup, State = #state{plist=PList, wdict=WordDict}) ->
  % We give up if these two conditions are met: a) everyone who hasn't quit
  % has voted to give, and b) at least one person has voted to give up.
  % That second condition ensures that the game isn't given up if everyone
  % leaves.
  case (lists:all(fun(#player{pid=Pid, gaveup=GaveUp}) ->
                      (Pid =:= quit) or GaveUp
                  end, PList)
        andalso lists:any(fun(#player{gaveup=G}) -> G end, PList)) of
    true ->
      announce_to_players(fun ws_handler:allgiveup/1, PList),
      NewWordDict = dict:map(fun(_Word, noone) -> "_";
                                (_Word, Guesser) -> Guesser
                             end, WordDict),
      {noreply, State#state{wdict=NewWordDict}};
    _ ->
      {noreply, State}
  end;

handle_cast(check_empty, State = #state{plist=PList, wset=WSet}) ->
  case sets:size(WSet) == 0
       andalso lists:all(fun(#player{pid=Pid}) -> Pid =:= quit end, PList) of
    true ->
      % set a destroy timer
      {ok, Timer} = timer:exit_after(?EMPTY_TIME, {timer_end, normal}),
      {noreply, State#state{destroy_ref={timer, Timer}}};
    _ ->
      {noreply, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', _TimerPid, {timer_end, TimerEndReason}}, State) ->
  {stop, TimerEndReason, State};

handle_info({'EXIT', FromPid, _Reason},
            State = #state{plist=PList, wset=WSet}) ->
  gen_server:cast(self(), check_empty),

  case sets:is_element(FromPid, WSet) of
    true ->
      NewWSet = sets:del_element(FromPid, WSet),
      {noreply, State#state{wset=NewWSet}};
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

      {noreply, State#state{plist=NewPList}}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{gamename=GameName}) ->
  lobby_mgr:destroy_game(GameName),
  ok;

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
