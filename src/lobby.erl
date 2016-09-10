%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2016 16:07
%%%-------------------------------------------------------------------
-module(lobby).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  register/0
  , leave/1]).

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


leave(Pid) ->
  gen_server:cast(?SERVER, {leave_lobby, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  io:format("lobby init~n"),
  {ok, #state{}}.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({register, Player1}, #state{waiting = undefined}=State) ->
  io:format("Player1 registered ~p~n",[Player1]),
  {noreply, State#state{waiting = Player1}};
handle_cast({register, Player2}, #state{waiting = Player1, games = Games}=State) ->
  {ok, Game} = match_gs:start(Player1, Player2),
  io:format("Player2 registered ~p; starting match ~p~n",[Player2, Game]),
  {noreply, State#state{waiting = undefined, games =  [Game | Games]}};
handle_cast({leave_lobby, UserPid}, State) ->
  {noreply, leave_lobby(UserPid,State)};
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

leave_lobby(UserPid, #state{waiting = UserPid}=State) ->
  State#state{waiting = undefined};
leave_lobby(_UserPid, State) ->
  % TODO figure out what to do here?
  % TODO maybe the match needs to ack with the connection to confirm if they are still around or not
  State.
