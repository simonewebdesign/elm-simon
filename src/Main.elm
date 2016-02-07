module Main where

import Array exposing (Array)
import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Color exposing (Color, red, yellow, green, blue)

import Keyboard
import Window

import Square


-- MODEL

type alias Model =
  { level : Int
  , score : Int
  , sequence : Array Int
  , inputSequence : Array Int
  , state : GameState
  , squares : List Square.Model
  }

type alias Dimensions = (Int, Int)
type alias Position = (Int, Int)

type Action = NoOp | ChangeGameState | InputSequence Int

type GameState = Play | Pause


initialModel : Model
initialModel =
  { level = 1
  , score = 0
  , sequence = Array.empty
  , inputSequence = Array.empty
  , state = Pause
  , squares =
    [ { id = 1, isActive = False, position = (-200, 160),  color = red }
    , { id = 2, isActive = False, position = (200, 160),   color = yellow }
    , { id = 3, isActive = False, position = (-200, -160), color = green }
    , { id = 4, isActive = False, position = (200, -160),  color = blue }
    ]
  }


-- UPDATE

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    ChangeGameState ->
      case model.state of
        Play  -> { model | state = Pause }
        Pause -> { model | state = Play }

    InputSequence id ->
      { model | inputSequence = Array.push id model.inputSequence }


-- VIEW

view : Dimensions -> Model -> Element
view (w, h) model =
  let
    debug = Element.show model |> Collage.toForm |> Collage.moveY 100

    squares = List.map (Square.view squaresMailbox.address) model.squares
  in
    Collage.collage w h ([debug] ++ squares)


-- PORTS & SIGNALS

squaresMailbox : Signal.Mailbox Int
squaresMailbox =
  Signal.mailbox 0


-- MAIN

main : Signal Element
main =
  Signal.map2 view Window.dimensions game


game : Signal Model
game =
  Signal.foldp update initialModel input


input : Signal Action
input =
  let
    --delta = Time.fps 30
    --toAction n =
    --  case n of
    --    -1 -> Subtract
    --    1 -> Add
    --    _ -> NoOp

    --arrows = Signal.sampleOn delta (Signal.map toAction x)


    --clicks = Signal.map (always Add) Mouse.clicks

    space = Signal.map (\pressed ->
      if pressed then
        ChangeGameState
      else
        NoOp
    ) Keyboard.space

    squarePresses = Signal.map InputSequence squaresMailbox.signal
  in
    Signal.mergeMany [space, squarePresses]
