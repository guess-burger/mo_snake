%%%-------------------------------------------------------------------
%%% @author gavinesberger
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 21:15
%%%-------------------------------------------------------------------
-module(websocket_handler).
-author("gavinesberger").

-define(PLAYER_ONE, p1).
-define(PLAYER_TWO, p2).

-record(state, { game = multi_snake_game:new(p1,p2), move = none}).

%% API
-export([
  init/3, terminate/3,
  websocket_init/3, websocket_terminate/3,
  websocket_info/3, websocket_handle/3
]).


init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.


websocket_init(_Type, Req, _Opts) ->
  io:format("Websocket init~n"),
  timer:send_interval(500, update),
  {ok, Req, #state{}}.


% Handles erlang messages
websocket_info(update, Req, #state{game = Game}=S) ->
  io:format("Game ~p~n",[Game]),
  UpdatedGame = multi_snake_game:step(Game),
  {reply, [
    {text, multi_snake_game:to_json(UpdatedGame)}
  ], Req, S#state{game = UpdatedGame}};
websocket_info(_Msg, Req, State) ->
  {ok, Req, State}.


% other frame types are text, binary, ping, pong
websocket_handle({text, Msg}, Req, State) ->
  NewMove = jsx:decode(Msg),
  io:format("Got msg: ~p~n",[NewMove]),
  Game = State#state.game,
  G2 = multi_snake_game:set_dir(?PLAYER_ONE, NewMove, Game),
  G3 = multi_snake_game:set_dir(?PLAYER_TWO, NewMove, G2),
  {ok, Req, State#state{game = G3}};
websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.


% Need to know if in a match or lobby
websocket_terminate(_Reason, _Req, _State) ->
  io:format("Websocket terminated~n"),
  ok.


terminate(_Reason, _Req, _State) ->
  ok.

