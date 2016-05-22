%%%-------------------------------------------------------------------
%%% @author gavinesberger
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2016 16:07
%%%-------------------------------------------------------------------
-module(lobby).
-author("gavinesberger").

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  register/0
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  waiting :: pid(),
  games = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


register() ->
  % TODO should this be a call and return a {lobby, Pid} or {game, Pid}
  % Not sure about that.
  % connections shouldn't know about each other but they do need to know
  % about the game or lobby since they need to let them know if the have
  % been disconnected. (does the websocket_handler need a FSM?)
  % should the handlers register with a match/game or should the match/game
  % inform the handlers?
  gen_server:cast(?SERVER, {register, self()}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({register, Player1}, #state{waiting = undefined}=State) ->
  {noreply, State#{waiting = Player1}};
handle_cast({register, Player2}, #state{waiting = Player1, games = Games}=State) ->
  {ok, Game} = match_gs:start_link(Player1, Player2),
  {noreply, State#state{waiting = undefined, games =  [Game | Games]}};
handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
