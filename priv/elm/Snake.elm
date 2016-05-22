import Graphics.Element exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Signal exposing (..)
import Window exposing (..)
import Keyboard
import Maybe

type alias Point = {x : Int, y : Int}
type alias Snake = {points : List Point, id : String, colour: String}
type alias State = {snakes: List Snake}

type Direction = Left | Right | Up | Down

-- incoming
port server_state : Signal State

-- outgoing
port directions : Signal String
port directions = directionSignal


directionSignal : Signal String
directionSignal =
    let arrowsToDelta {x,y} =
            case (x,y) of
                ( 0, 0) -> Nothing
                ( -1, _) -> Just "Left"
                ( 1, _) -> Just "Right"
                ( _, 1) -> Just "Up"
                ( _, -1) -> Just "Down"
                (_, _) -> Nothing
    in Signal.filterMap arrowsToDelta "Right" Keyboard.wasd


render: (Int, Int) -> State -> Element
render (w,h) state =
    color grey
        <| container w h middle
        <| color white
        <| collage 300 300 (List.concatMap render_snake state.snakes)

render_snake snake =
    let
        colour = to_colour snake.colour
        render_coloured_point = render_point colour
    in
    List.map render_coloured_point snake.points

to_colour colour =
    case colour of
        "red" -> red
        "blue" -> blue
        _ -> green


render_point colour point =
    let size = 25
        xStart = size * -6
        yStart = size * 6
    in square size
        |> filled colour
        |> move (toFloat (xStart + size*point.x), toFloat (yStart - size*point.y))


main =
    Signal.map2 render Window.dimensions server_state