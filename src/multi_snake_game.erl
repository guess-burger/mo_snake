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
  set_dir/3,
  step/1,
  force_winner/2,
  is_over/1, result/2,
  to_json/1, win_json/1, lose_json/1, draw_json/1
]).

-record(snake, {
  id,
  dir_press,
  dir,
  points,
  colour
}).

-record(game, {
  is_over = false :: false | {true, draw | any()},
  snake1 :: #snake{},
  snake2 :: #snake{},
  pellet
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
  P1Points = queue:from_list([{1, 5}, {2, 5}, {3, 5}]),
  P1Snake = #snake{id = Player1, dir_press = ?NONE, dir = ?RIGHT, points = P1Points, colour = <<"red">>},

  P2Points = queue:from_list([{1, 19}, {2, 19}, {3, 19}]),
  P2Snake = #snake{id = Player2, dir_press = ?NONE, dir = ?RIGHT, points = P2Points, colour = <<"blue">>},

  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),

  new_pellet(#game{snake1 = P1Snake, snake2 = P2Snake}).


is_over(#game{is_over = false}) ->
  false;
is_over(#game{is_over = {true, _}}) ->
  true.

result(#game{is_over = false}, _Player) ->
  not_over;
result(#game{is_over = {true, Result}}, Player) ->
  case Result of
    draw -> draw;
    Player -> win;
    _OtherPlayer -> lose
  end.


set_dir(SnakeId, Dir, #game{snake1 = #snake{id = SnakeId} = Snake1} = State) ->
  NewSnake1 = Snake1#snake{dir_press = Dir},
  State#game{snake1 = NewSnake1};
set_dir(SnakeId, Dir, #game{snake2 = #snake{id = SnakeId} = Snake2} = State) ->
  NewSnake1 = Snake2#snake{dir_press = Dir},
  State#game{snake2 = NewSnake1}.


step(#game{snake1 = Snake1, snake2 = Snake2, pellet = Pellet} = Game) ->

  % really need the pellets to be visible in the snake so that
  % all is fair
  % that means pellets need to be smaller than the snake blocks


  {Head1, CrashSelf1, #snake{points = SnakeQ1}=NewSnake1} = step_snake(Snake1),
  {Head2, CrashSelf2, #snake{points = SnakeQ2}=NewSnake2} = step_snake(Snake2),

  NewGame =
    case Pellet of
      Head1 ->
        WithPellet = NewSnake1#snake{points = queue:in(Pellet, SnakeQ1)},
        new_pellet(Game#game{snake1 = WithPellet, snake2 = NewSnake2});
      Head2 ->
        WithPellet = NewSnake2#snake{points = queue:in(Pellet, SnakeQ2)},
        new_pellet(Game#game{snake1 = NewSnake1, snake2 = WithPellet});
      _ ->
        Game#game{snake1 = NewSnake1, snake2 = NewSnake2}
    end,

  Crash1 = queue:member(Head1, SnakeQ2),
  Crash2 = queue:member(Head2, SnakeQ1),

  case {Crash1 or CrashSelf1, Crash2 or CrashSelf2} of
    {true, true} ->
      io:format("Draw ~p ~p~n", [queue:to_list(SnakeQ1), queue:to_list(SnakeQ2)]),
      NewGame#game{is_over = {true, draw}};
    {true, false} ->
      io:format("P2 Win ~n"),
      NewGame#game{is_over = {true, NewSnake2#snake.id}};
    {false, true} ->
      io:format("P1 Win ~n"),
      NewGame#game{is_over = {true, NewSnake1#snake.id}};
    _NoCrashes ->
      NewGame
  end.

step_snake(Snake) ->
  S2 = update_dir(Snake),
  #snake{points = Points, dir = Dir} = S2,

  Head = queue:get_r(Points),
  NewHead = newHead(Head, Dir),

  Moved = queue:drop(Points),
  CrashSelf = queue:member(NewHead, Moved),
  {NewHead, CrashSelf, S2#snake{points = queue:in(NewHead, Moved), dir_press = ?NONE} }.

update_dir(#snake{dir_press = ?NONE} = State) ->
  State;
update_dir(#snake{dir_press = ?LEFT, dir = Old} = State) when Old =/= ?RIGHT ->
  State#snake{dir = ?LEFT};
update_dir(#snake{dir_press = ?RIGHT, dir = Old} = State) when Old =/= ?LEFT ->
  State#snake{dir = ?RIGHT};
update_dir(#snake{dir_press = ?UP, dir = Old} = State) when Old =/= ?DOWN ->
  State#snake{dir = ?UP};
update_dir(#snake{dir_press = ?DOWN, dir = Old} = State) when Old =/= ?UP ->
  State#snake{dir = ?DOWN};
update_dir(State) ->
  State.

% FIXME there must be a better way of doing this! Just add then mod!
newHead({?WIDTH, Y}, ?RIGHT) ->
  {1, Y};
newHead({X, Y}, ?RIGHT) ->
  {X + 1, Y};
newHead({1, Y}, ?LEFT) ->
  {?WIDTH, Y};
newHead({X, Y}, ?LEFT) ->
  {X - 1, Y};
newHead({X, 1}, ?UP) ->
  {X, ?HEIGHT};
newHead({X, Y}, ?UP) ->
  {X, Y - 1};
newHead({X, ?HEIGHT}, ?DOWN) ->
  {X, 1};
newHead({X, Y}, ?DOWN) ->
  {X, Y + 1}.

new_pellet(#game{snake1 = #snake{points = Snake1}, snake2 = #snake{points = Snake2}} = Game) ->
  % FIXME this function could be better. Issues with it are:
  % * players might take up the whole screen
  % * if players take a lot of space this function might run for a while
  PotentialPellet = {random:uniform(?WIDTH), random:uniform(?HEIGHT)},

  InS1 = queue:member(PotentialPellet, Snake1),
  InS2 = queue:member(PotentialPellet, Snake2),

  if
    InS1 -> new_pellet(Game);
    InS2 -> new_pellet(Game);
    true -> Game#game{ pellet = PotentialPellet}
  end.


force_winner(Winner, Game) ->
  Game#game{ is_over = {true, Winner}}.


to_json(#game{snake1 = Snake1, snake2 = Snake2, pellet = Pellet}) ->
  PelletJson = make_snake(points_to_json([Pellet]), <<"black">>),
  JsonSnakes = [snake_to_json(Snake1), snake_to_json(Snake2), PelletJson],
  jsx:encode([{<<"snakes">>, JsonSnakes}]).

snake_to_json(#snake{points = Points, colour = Colour}) ->
  JsonPoints = points_to_json(queue:to_list(Points)),
  make_snake(JsonPoints, Colour).

make_snake(Points, Colour) ->
  [{<<"points">>, Points}, {<<"colour">>, Colour}].

points_to_json(Points) ->
  [[{<<"x">>, X}, {<<"y">>, Y}] || {X, Y} <- Points].


win_json(#game{snake1 = Snake1, snake2 = Snake2}) ->
  Points = [{X + 3, Y + 8} || {X, Y} <- win_points()],
  TextSnake = make_snake(points_to_json(Points), <<"black">>),
  JsonSnakes = [snake_to_json(Snake1), snake_to_json(Snake2)],
  jsx:encode([{<<"snakes">>, [TextSnake | JsonSnakes]}]).

win_points() ->
  % Thanks to http://www.dafont.com/pixeltext.font?text=Win+Lose+Draw
  [
    % W
    {1, 1}, {1, 2}, {1, 3}, {1, 4},
    {2, 5}, {3, 4}, {4, 5},
    {5, 1}, {5, 2}, {5, 3}, {5, 4},

    % I
    {7, 1}, {7, 5},
    {8, 1}, {8, 2}, {8, 3}, {8, 4}, {8, 5},
    {9, 1}, {9, 5},

    % N
    {11, 1}, {11, 2}, {11, 3}, {11, 4}, {11, 5},
    {12, 2}, {13, 3}, {14, 4},
    {15, 1}, {15, 2}, {15, 3}, {15, 4}, {15, 5},

    % !
    {17, 1}, {17, 2}, {17, 3}, {17, 5}
  ].


lose_json(#game{snake1 = Snake1, snake2 = Snake2}) ->
  Points = [{X + 3, Y + 8} || {X, Y} <- lose_points()],
  TextSnake = make_snake(points_to_json(Points), <<"black">>),
  JsonSnakes = [snake_to_json(Snake1), snake_to_json(Snake2)],
  jsx:encode([{<<"snakes">>, [TextSnake | JsonSnakes]}]).

lose_points() ->
  [
    % L
    {1, 5}, {1, 4}, {1, 3}, {1, 2}, {1, 1},
    {2, 5}, {3, 5},

    % O
    {5, 4}, {5, 3}, {5, 2},
    {6, 5}, {6, 1},
    {7, 4}, {7, 3}, {7, 2},

    % S
    {9, 5}, {9, 3}, {9, 2}, {9, 1},
    {10, 5}, {10, 3}, {10, 1},
    {11, 5}, {11, 4}, {11, 3}, {11, 1},

    % E
    {13, 5}, {13, 4}, {13, 3}, {13, 2}, {13, 1},
    {14, 5}, {14, 3}, {14, 1},
    {15, 5}, {15, 3}, {15, 1}
  ].


draw_json(#game{snake1 = Snake1, snake2 = Snake2}) ->
  Points = [{X + 4, Y + 8} || {X, Y} <- draw_points()],
  TextSnake = make_snake(points_to_json(Points), <<"black">>),
  JsonSnakes = [snake_to_json(Snake1), snake_to_json(Snake2)],
  jsx:encode([{<<"snakes">>, [TextSnake | JsonSnakes]}]).

draw_points() ->
  [
    % D
    {1, 1}, {1, 2}, {1, 3}, {1, 4}, {1, 5},
    {2, 1}, {2, 5},
    {3, 2}, {3, 3}, {3, 4},

    % R
    {5, 1}, {5, 2}, {5, 3}, {5, 4}, {5, 5},
    {6, 4}, {6, 3}, {6, 1},
    {7, 5}, {7, 3}, {7, 2},

    % A
    {9, 5}, {9, 4}, {9, 3}, {9, 2},
    {10, 3}, {10, 1},
    {11, 5}, {11, 4}, {11, 3}, {11, 2},

    % W
    {13, 5}, {13, 4}, {13, 3}, {13, 2}, {13, 1},
    {14, 4},
    {15, 5}, {15, 4}, {15, 3}, {15, 2}, {15, 1}

  ].