%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2015 21:15
%%%-------------------------------------------------------------------
% TODO should this module have a better name? Something about users in it? at least something less generic
-module(websocket_handler).

% TODO is match the right name. I think we should have the lobbies listed here as we're using this to let an
% owner know this user has quit. Does this mean we need to have a match that knows about the user and a user that
% knows about th match? Maybe but in an actor world is that that bad of an idea?
% for now lets just have it so if the match is undefined then we'll send the ned message to the lobby
-record(state, {
  match,
  match_ref
}).

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
  lobby:register(),
  {ok, Req, #state{}}.


% Handles erlang messages
websocket_info({update, Game}, Req, State) ->
  {reply, [
    {text, multi_snake_game:to_json(Game)}
  ], Req, State};
websocket_info({match_start, Match}, Req, State) ->
  Ref = monitor(process, Match),
  {ok, Req, State#state{match = Match, match_ref = Ref}};
websocket_info(join_lobby, Req, State) ->
  lobby:register(),
  {ok, Req, State};
websocket_info(won, Req, State) ->
  % FIXME to use the same mechanism that normal winning does
  erlang:send_after(2000, self(), join_lobby),
  WinJson = multi_snake_game:win_json(),
  {reply, {text, WinJson}, Req, State#state{match = undefined}};
websocket_info({'DOWN',MatchRef,_,Match,_}, Req, #state{match = Match, match_ref = MatchRef}=State) ->
  % TODO this is the case where the match just crashes for whatever reason
  % we don' match on this when the match is over since the match becomes undefined not the pid!
  io:format("Match down~n"),
  lobby:register(),
  {ok, Req, State#state{match=undefined, match_ref = undefined}};
websocket_info({gameover, Game}, Req, State) ->
  erlang:send_after(5000, self(), join_lobby),
  % TODO is this too leaky? Should this know about the game module at all?
  Json =
    case multi_snake_game:result(Game,self()) of
      draw -> multi_snake_game:draw_json();
      lose -> multi_snake_game:lose_json();
      win -> multi_snake_game:win_json(Game)
    end,
  {reply, {text, Json}, Req, State#state{match = undefined}};
websocket_info(Msg, Req, State) ->
  io:format("Got Unknown ~p~p~n", [Msg,State]),
  {ok, Req, State}.


% other frame types are text, binary, ping, pong
websocket_handle({text, Msg}, Req, #state{ match = GamePid }=State) when GamePid =/= undefined->
  % TODO we only want to handle this when we're in a game!
  % if we allow spectator then this needs more FSM!!!!
  NewMove = jsx:decode(Msg),
  io:format("Got msg: ~p~n",[NewMove]),
  match_gs:move(NewMove,GamePid),
  {ok, Req, State};
websocket_handle(_Frame, Req, State) ->
  {ok, Req, State}.



websocket_terminate(_Reason, _Req, #state{match = undefined}) ->
  % We're waiting in the lobby!
  lobby:leave(self()),
  ok;
websocket_terminate(_Reason, _Req, #state{match = Match}) ->
  io:format("Websocket terminated~n"),
  match_gs:leave(Match),
  ok.


terminate(_Reason, _Req, _State) ->
  ok.

