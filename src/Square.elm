module Square where

import Graphics.Element as Element exposing (Element)
import Graphics.Collage as Collage
import Graphics.Input   as Input
import Color exposing (Color)

type alias Model =
  { id : Int
  , isActive : Bool
  , position : (Float, Float)
  , color : Color
  }


view : Signal.Address Int -> Model -> Collage.Form
view address model =
  Element.empty
  |> Element.size 200 200
  |> Element.color model.color
  |> Element.opacity (if model.isActive then 1 else 0.2)
  |> Input.clickable (Signal.message address model.id)
  |> Collage.toForm
  |> Collage.move model.position
