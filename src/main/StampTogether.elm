module StampTogether where

import Signal (..)
import Signal
import Time (..)
import List (..)
import List
import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Text (..)
import Window
import Mouse

main : Signal Element
main = state |> Signal.map .value |> Signal.map2 view Window.dimensions

-- State

port state : Signal Model

type alias Model = {
  url: String,
  value: List Stamp
}

type alias Stamp = {
  x: Float,
  y: Float
}

port observedUrls : Signal (List String)
port observedUrls = every (0.1 * second) |> foldp (\_ count -> count + 1) 0 |> Signal.map getUrl |> dropRepeats

getUrl : Int -> List String
getUrl count = if count == 0 then [] else [url]

url = "https://thsoft.firebaseio-demo.com/StampTogether"

-- View

view : (Int, Int) -> List Stamp -> Element
view (windowWidth, windowHeight) stamps =
  stamps |> List.map (viewStamp windowWidth windowHeight) |> collage windowWidth windowHeight

viewStamp : Int -> Int -> Stamp -> Form
viewStamp windowWidth windowHeight {x, y} =
  let color = hsla (y |> degrees) 1 0.5 0.7
      screenX = x - (windowWidth |> toFloat) / 2
      screenY = (windowHeight |> toFloat) / 2 - y
      angle = x |> degrees
  in ngon 5 20 |> filled color |> move (screenX, screenY) |> rotate angle

-- Commands

port createStamp : Signal CreateStamp
port createStamp = Mouse.position |> sampleOn Mouse.clicks |> Signal.map makeCreateStamp

type alias CreateStamp = {
  url: String,
  value: Stamp
}

makeCreateStamp : (Int, Int) -> CreateStamp
makeCreateStamp (x, y) =
  {
    url = url,
    value = {
      x = x |> toFloat,
      y = y |> toFloat
    }
  }