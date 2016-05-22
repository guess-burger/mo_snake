%%%-------------------------------------------------------------------
%%% @author gavinesberger
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2015 14:07
%%%-------------------------------------------------------------------
-module(multi_snake_game).
-author("gavinesberger").

%% API
-export([
  new/2,
  step/1,
  is_over/1,
  set_dir/3,
  to_json/1
]).

-record(snake, {
  dir_press,
  dir,
  points,
  colour
}).

-record(game, {
  is_over,
  snakes :: #{any() => #snake{}}
}).

-define(WIDTH, 11).
-define(HEIGHT, 11).
-define(LEFT, <<"Left">>).
-define(RIGHT, <<"Right">>).
-define(UP, <<"Up">>).
-define(DOWN, <<"Down">>).
-define(NONE, none).


new(Player1, Player2) ->
  P1Points = queue:from_list([{3,3},{2,3},{1,3}]),
  P1Snake = #snake{dir_press = ?NONE, dir = ?RIGHT, points = P1Points, colour = <<"red">>},

  P2Points = queue:from_list([{3,8},{2,8},{1,8}]),
  P2Snake = #snake{dir_press = ?NONE, dir = ?RIGHT, points = P2Points, colour = <<"blue">>},

  #game{is_over = false, snakes = #{Player1=>P1Snake, Player2=>P2Snake}}.

is_over(#game{is_over = IsOver}) ->
  IsOver.


set_dir(SnakeId, Dir, #game{snakes=Snakes}=State) ->
  #{SnakeId := Snake} = Snakes,
  Snake2 = Snake#snake{dir_press = Dir},
  State#game{snakes = Snakes#{SnakeId=>Snake2}}.

step(#game{snakes = Snakes}=G) ->

  % FIXME no sort of collision detection yet!
  Snakes2 = maps:map(fun step_snake/2, Snakes),

  G#game{snakes =  Snakes2}.

step_snake(_Id, Snake)->
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

to_json(#game{ snakes = Snakes}) ->
  SnakesList = maps:to_list(Snakes),
  JsonSnakes = [ snake_to_json(Id, Snake) || {Id, Snake} <- SnakesList],
  jsx:encode([{<<"snakes">>, JsonSnakes}]).

snake_to_json(Id, #snake{points = Points, colour = Colour}) ->
  JsonPoints = points_to_json(Points),
  [{<<"id">>, Id}, {<<"points">>, JsonPoints}, {<<"colour">>, Colour}].

points_to_json(Points) ->
  [ [{<<"x">>, X}, {<<"y">>, Y}] || {X,Y} <- queue:to_list(Points)].
