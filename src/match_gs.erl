%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2016 21:00
%%%-------------------------------------------------------------------
-module(match_gs).

-behaviour(gen_server).

%% API
-export([start/2, move/2, leave/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(init_state, {
  player1 :: pid(),
  player2 :: pid(),
  count_down = 3
}).

% FIXME is this too much like a FSM to not be a gen_fsm?
-record(playing_state, {
  player1 :: pid(),
  player2 :: pid(),
  game :: multi_snake_game:multi_snake_game()
}).

%%%===================================================================
%%% API
%%%===================================================================


start(Player1, Player2) ->
  % FIXME this can't handle more than one match at the minute
  % really need to have simple one for one
  % Or maybe don't name them the same thing
  % Going to not name them for now with the intention of simple 1 for 1 (why? what does it give us? also should i use start link then? probs not)

  gen_server:start(?MODULE, {Player1, Player2}, []).


move(Dir, Game) ->
  gen_server:cast(Game, {move, self(), Dir}).


leave(Game) ->
  % TODO should this be a cast or call
  gen_server:cast(Game, {player_left, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Player1, Player2}) ->
  Player1 ! {match_start, self()},
  Player2 ! {match_start, self()},
  % TODO confirm if this is a good idea
  erlang:send(self(), tick),
  io:format("Match init~n"),
  {ok, #init_state{player1 = Player1, player2 = Player2}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({player_left, PlayerPid}, State) ->
  GameOverState = player_left(PlayerPid, State),
  {stop, normal, GameOverState};
handle_cast({move, Player, Direction}, #playing_state{ game = Game }=State) ->
  Game2 = multi_snake_game:set_dir(Player, Direction, Game),
  {noreply, State#playing_state{game = Game2}};
handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(tick, #init_state{player1 = P1, player2 = P2}) ->
  % TODO really need to count the match in (3..2..1..GO)
  Game = multi_snake_game:new(P1,P2),
  % FIXME do we need to cancel this at some point?
  timer:send_interval(500, tick),
  io:format("Match start~n"),
  {noreply, #playing_state{player1=P1, player2 = P2, game = Game }};
handle_info(tick, #playing_state{game = Game}=State) ->
  io:format("Tick~n"),
  UpdatedGame = multi_snake_game:step(Game),
  State#playing_state.player1 ! {update, UpdatedGame},
  State#playing_state.player2 ! {update, UpdatedGame},
  {noreply, State#playing_state{game = UpdatedGame}};
handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO do we need to return any state?
player_left(PlayerPid, #init_state{player1 = P1, player2 = P2}=State) ->
  player_left(P1,P2,PlayerPid),
  State;
player_left(PlayerPid, #playing_state{player1 = P1, player2 = P2}=State) ->
  player_left(P1,P2,PlayerPid),
  State.

player_left(P1, P2, PlayerLeft) when P1 == PlayerLeft->
  io:format("Player 1 ~p has left the match~n", [PlayerLeft]),
  P2 ! won, % TODO should this be wrapped in a call in websocket_handler?
  ok;
player_left(P1, P2, PlayerLeft) when P2 == PlayerLeft->
  io:format("Player 2 ~p has left the match~n", [PlayerLeft]),
  P1 ! won,
  ok.