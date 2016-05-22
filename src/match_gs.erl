%%%-------------------------------------------------------------------
%%% @author gavinesberger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2016 21:00
%%%-------------------------------------------------------------------
-module(match_gs).
-author("gavinesberger").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  player1 :: pid(),
  player2 :: pid(),
  game
}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Player1, Player2) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Player1, Player2}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Player1, Player2}) ->
  Game = multi_snake_game:new(Player1,Player2),
  Player1 ! {match_start, self()},
  Player2 ! {match_start, self()},
  timer:send_interval(500, tick),
  {ok, #state{player1 = Player1, player2 = Player2, game = Game}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(tick, #state{game = Game}=State) ->
  UpdatedGame = multi_snake_game:step(Game),
  State#state.player1 ! {update, UpdatedGame},
  State#state.player2 ! {update, UpdatedGame},
  {noreply, State#state{game = UpdatedGame}};
handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
