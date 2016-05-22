%%%-------------------------------------------------------------------
%%% @author gavinesberger
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2015 14:07
%%%-------------------------------------------------------------------
-module(snake_game).
-author("gavinesberger").

%% API
-export([
  new/0,
  step/1,
  is_over/1,
  set_dir/2,
  to_json/1
]).

-record(snake, {
  dir_press,
  dir,
  points
}).

-record(game, {is_over, snake_1}).

-define(WIDTH, 11).
-define(HEIGHT, 11).
-define(LEFT, <<"Left">>).
-define(RIGHT, <<"Right">>).
-define(UP, <<"Up">>).
-define(DOWN, <<"Down">>).
-define(NONE, none).

% FIXME this is all coded for one snake! How will we handle multiple snakes?
% I'd really like t be able to add and remove snakes as and when required
% That sounds like a map of snakes. Could use process in future.

new() ->
  Points = queue:from_list([{3,1},{2,1},{1,1}]),
  Snake = #snake{dir_press = ?NONE, dir = ?RIGHT, points = Points},
  #game{is_over = false, snake_1 = Snake}.

is_over(#game{is_over = IsOver}) ->
  IsOver.


set_dir(Dir, State) ->
  Snake = State#game.snake_1,
  S2 = Snake#snake{dir_press = Dir},
  State#game{snake_1 = S2}.

step(#game{snake_1 = S}=G) ->
  S2 = update_dir(S),
  #snake{points = Points, dir = Dir}=S2,

  Head = queue:get(Points),
  NewHead = newHead(Head,Dir),

  Moved = queue:drop_r(Points),
  UpdateSnake = S2#snake{ points = queue:in_r(NewHead, Moved), dir_press = ?NONE},
  G#game{snake_1 = UpdateSnake}.

update_dir(#snake{dir_press = ?NONE}=State) ->
  State;
update_dir(#snake{dir_press = ?LEFT, dir=Old}=State) when Old =/= ?RIGHT->
  State#snake{dir = ?LEFT};
update_dir(#snake{dir_press = ?RIGHT, dir=Old}=State) when Old =/= ?LEFT->
  State#snake{dir = ?RIGHT};
update_dir(#snake{dir_press = ?UP, dir=Old}=State) when Old =/= ?DOWN->
  State#snake{dir = ?UP};
update_dir(#snake{dir_press = ?DOWN, dir=Old}=State) when Old =/= ?UP->
  State#snake{dir = ?DOWN};
update_dir(State)->
  State.

newHead({?WIDTH,Y}, ?RIGHT) ->
  {1,Y};
newHead({X,Y}, ?RIGHT) ->
  {X+1,Y};
newHead({1,Y}, ?LEFT) ->
  {?WIDTH,Y};
newHead({X,Y}, ?LEFT) ->
  {X-1,Y};
newHead({X,1}, ?UP) ->
  {X,?HEIGHT};
newHead({X,Y}, ?UP) ->
  {X,Y-1};
newHead({X,?HEIGHT}, ?DOWN) ->
  {X,1};
newHead({X,Y}, ?DOWN) ->
  {X,Y+1}.

to_json(#game{ snake_1 = #snake{points = Points}}) ->
  JsonPoints = points_to_json(Points),
  jsx:encode([ {<<"points">>, JsonPoints} ]).

points_to_json(Points) ->
  [ [{<<"x">>, X},{<<"y">>, Y}] || {X,Y} <- queue:to_list(Points)].
