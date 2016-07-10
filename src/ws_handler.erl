-module(ws_handler).

%% API
-export([player_list/2]).
-export([word_list/2]).
-export([word_guessed/3]).
-export([player_joined/2]).
-export([player_quit/2]).
-export([player_giveup/3]).
-export([allgiveup/1]).

%% cowboy
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% API
-spec player_list(identifier(), [{string(), boolean()}]) -> any().
player_list(Pid, PList) ->
  Pid ! {player_list, PList}.

-spec word_list(identifier(), [{string(), boolean()}]) -> any().
word_list(Pid, WList) ->
  Pid ! {word_list, WList}.

-spec word_guessed(identifier(), string(), string()) -> any().
word_guessed(Pid, Word, Guesser) ->
  Pid ! {word_guessed, Word, Guesser}.

-spec player_joined(identifier(), string()) -> any().
player_joined(Pid, Name) ->
  Pid ! {player_joined, Name}.

-spec player_quit(identifier(), string()) -> any().
player_quit(Pid, Name) ->
  Pid ! {player_quit, Name}.

-spec player_giveup(identifier(), string(), boolean()) -> any().
player_giveup(Pid, Name, Status) ->
  Pid ! {player_giveup, Name, Status}.

-spec allgiveup(identifier()) -> any().
allgiveup(Pid) ->
  Pid ! allgiveup.

%% cowboy
init(Req, _Opts) ->
  {cowboy_websocket, Req, nogame}.

websocket_handle({text, Msg}, Req, nogame) ->
  LMsg = binary_to_list(Msg),
  case string:tokens(LMsg, " ") of
    [":host", Playername] ->
      case lobby_mgr:new_coop_game(Playername) of
        badname ->
          {reply, [{text, <<":badname">>}, close], Req, nogame};
        {ok, NewGamePid} ->
          {ok, Req, {want_words, NewGamePid}}
      end;
    [":join", GameName] ->
      case lobby_mgr:find_game(GameName) of
        noexist ->
          {reply, [{text, <<":noexist">>}, close], Req, nogame};
        {ok, GamePid} ->
          {reply, {text, <<":ok">>}, Req, {want_name, GamePid}}
      end
  end;
websocket_handle({text, Msg}, Req, {want_name, GamePid}) ->
  Name = binary_to_list(Msg),
  case lobby_mgr:validate_name(Name) of
    true ->
      case coop_game:join_game(GamePid, Name) of
        taken ->
          {reply, {text, <<":taken">>}, Req, {want_name, GamePid}};
        ok ->
          {ok, Req, {want_playerlist, GamePid}}
      end;
    false ->
      {reply, {text, <<":badname">>}, Req, {want_name, GamePid}}
  end;

websocket_handle({text, Msg}, Req, State = {want_words, GamePid}) ->
  LMsg = binary_to_list(Msg),
  case string:tokens(LMsg, " ") of
    [":addword", Word, Guessed] ->
      IsGuessed = case Guessed of
                    "y" -> true;
                    "n" -> false
                  end,
      coop_game:add_word(GamePid, Word, IsGuessed),
      {ok, Req, State};
    [":endwords"] ->
      GameName = coop_game:end_words(GamePid),
      {reply, {text, GameName}, Req, {playing, GamePid}}
  end;

websocket_handle({text, Msg}, Req, State = {playing, GamePid}) ->
  LMsg = binary_to_list(Msg),
  case string:tokens(LMsg, " ") of
    [":attempt", Word] ->
      coop_game:attempt_word(GamePid, Word);
    [":giveup"] ->
      coop_game:giveup_status(GamePid, true);
    [":ungiveup"] ->
      coop_game:giveup_status(GamePid, false)
  end,
  {ok, Req, State};
      
websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.

websocket_info({player_list, PList}, Req, {want_playerlist, GamePid}) ->
  ToSend = lists:map(fun({Name, Present}) ->
                         {text,
                          [<<":player ">>,
                           case Present of
                             true -> <<"y ">>;
                             false -> <<"n ">>
                           end,
                           list_to_binary(Name)]}
                     end, PList)
  ++ [{text, <<":endplayers">>}],
  {reply, ToSend, Req, {want_wordlist, GamePid}};

websocket_info({word_list, WList}, Req, {want_wordlist, GamePid}) ->
  ToSend = lists:map(fun(W) -> {text,
                                [<<":word ">>, list_to_binary(W)]}
                     end, WList)
  ++ [{text, <<":endwords">>}],
  {reply, ToSend, Req, {playing, GamePid}};

websocket_info({word_guessed, Word, Guesser}, Req, State) ->
  {reply,
   {text, [<<":attempt ">>,
                  list_to_binary(Word),
                  <<" ">>,
                  list_to_binary(Guesser)]},
   Req, State};

websocket_info({player_joined, Name}, Req, State) ->
  {reply, {text, [<<":join ">>, list_to_binary(Name)]}, Req, State};

websocket_info({player_quit, Name}, Req, State) ->
  {reply, {text, [<<":quit ">>, list_to_binary(Name)]}, Req, State};

websocket_info({player_giveup, Name, Status}, Req, State) ->
  Action = case Status of
             true ->
               <<":giveup ">>;
             _ ->
               <<":ungiveup ">>
           end,
  {reply, {text, [Action, list_to_binary(Name)]}, Req, State};

websocket_info(allgiveup, Req, State) ->
  {reply, {text, <<":allgiveup">>}, Req, State};

websocket_info(Info, Req, State) ->
  io:fwrite("unkown msg ~p~nwith state ~p~n", [Info, State]),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
