%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2015 14:07
%%%-------------------------------------------------------------------
-module(multi_snake_game).

%% API
-export([
  new/2,
  step/1,
  is_over/1,
  set_dir/3,
  to_json/1,
  win_json/0
]).

-record(snake, {
  id,
  dir_press,
  dir,
  points,
  colour
}).

-record(game, {
  is_over = false :: boolean(),
  snake1 :: #snake{},
  snake2 :: #snake{}
}).

-opaque multi_snake_game() :: #game{}.

-export_type([multi_snake_game/0]).


-define(WIDTH, 23).
-define(HEIGHT, 23).
-define(LEFT, <<"Left">>).
-define(RIGHT, <<"Right">>).
-define(UP, <<"Up">>).
-define(DOWN, <<"Down">>).
-define(NONE, none).


new(Player1, Player2) ->
  P1Points = queue:from_list([{3,4},{2,4},{1,4}]),
  P1Snake = #snake{id = Player1, dir_press = ?NONE, dir = ?RIGHT, points = P1Points, colour = <<"red">>},

  P2Points = queue:from_list([{3,20},{2,20},{1,20}]),
  P2Snake = #snake{id = Player2, dir_press = ?NONE, dir = ?RIGHT, points = P2Points, colour = <<"blue">>},

  #game{ snake1 = P1Snake, snake2 = P2Snake} .


is_over(#game{is_over = IsOver}) ->
  IsOver.


set_dir(SnakeId, Dir, #game{snake1 = #snake{id = SnakeId}=Snake1}=State) ->
  NewSnake1 = Snake1#snake{dir_press = Dir},
  State#game{snake1 = NewSnake1};
set_dir(SnakeId, Dir, #game{snake2 = #snake{id = SnakeId}=Snake2}=State) ->
  NewSnake1 = Snake2#snake{dir_press = Dir},
  State#game{snake2 = NewSnake1}.


step(#game{snake1 = Snake1, snake2 = Snake2}=G) ->

  % TODO add collision detection! and pellets!

  % TODO need to decide the rules of the game!
  % should there be only one pellet?
  % should there be points?
  % should the other person win if you crash into them?
  % is head hitting head a draw?

  % really need the pellets to be visible in the snake so that
  % all is fair
  % that means pellets need to be smaller than the snake blocks


  NewSnake1 = step_snake(Snake1),
  NewSnake2 = step_snake(Snake2),

  G#game{snake1 = NewSnake1, snake2 = NewSnake2}.

step_snake(Snake)->
  S2 = update_dir(Snake),
  #snake{points = Points, dir = Dir}=S2,

  Head = queue:get(Points),
  NewHead = newHead(Head,Dir),

  Moved = queue:drop_r(Points),
  S2#snake{ points = queue:in_r(NewHead, Moved), dir_press = ?NONE}.

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


to_json(#game{ snake1 = Snake1, snake2 = Snake2}) ->
  JsonSnakes = [ snake_to_json(Snake1), snake_to_json(Snake2)],
  jsx:encode([{<<"snakes">>, JsonSnakes}]).

snake_to_json(#snake{points = Points, colour = Colour}) ->
  JsonPoints = points_to_json(queue:to_list(Points)),
  make_snake(JsonPoints, Colour).

make_snake(Points, Colour) ->
  [{<<"points">>, Points}, {<<"colour">>, Colour}].

points_to_json(Points) ->
  [ [{<<"x">>, X}, {<<"y">>, Y}] || {X,Y} <- Points].


win_json() ->
  Points = [ {X+3, Y+8} || {X,Y} <- win_points() ],
  TextSnake = make_snake(points_to_json(Points),<<"black">>),
  jsx:encode([{<<"snakes">>, [TextSnake]}]).

win_points() ->
  [
    % W
    {1,1}, {1,2}, {1,3}, {1,4},
    {2,5}, {3,4}, {4,5},
    {5,1}, {5,2}, {5,3}, {5,4},

    % I
    {7,1}, {7,5},
    {8,1}, {8,2}, {8,3}, {8,4}, {8,5},
    {9,1}, {9,5},

    % N
    {11,1}, {11,2}, {11,3}, {11,4}, {11,5},
    {12,2}, {13,3}, {14,4},
    {15,1}, {15,2}, {15,3}, {15,4}, {15,5},

    % !
    {17,1}, {17,2}, {17,3}, {17,5}
  ].
