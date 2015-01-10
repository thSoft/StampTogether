module StampTogether where

import Signal (..)
import Signal
import Time (..)
import List (..)
import List
import Color (..)
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Text (..)
import Text
import Window
import Mouse

main : Signal Element
main = state |> Signal.map2 view Window.dimensions

-- State

port state : Signal Model

type alias Model = List Stamp

type alias Stamp = {
  url: String,
  value: Position
}

type alias Position = {
  x: Float,
  y: Float
}

port observedUrls : Signal (List String)
port observedUrls = every (0.1 * second) |> foldp (\_ count -> count + 1) 0 |> Signal.map getUrl |> dropRepeats

getUrl : Int -> List String
getUrl count = if count == 0 then [] else [url]

url = "https://thsoft.firebaseio-demo.com/StampTogether"

-- View

view : (Int, Int) -> Model -> Element
view (windowWidth, windowHeight) stamps =
  stamps |> List.map (viewStamp windowWidth windowHeight) |> collage windowWidth windowHeight

viewStamp : Int -> Int -> Stamp -> Form
viewStamp windowWidth windowHeight stamp =
  let angle = (stamp.value.x * stamp.value.y) |> degrees
      color = hsla angle 1 0.5 0.7
      stampSize = radius * 2
      delete = stamp.url |> send deleteStampChannel
      screenX = stamp.value.x - radius - (windowWidth |> toFloat) / 2
      screenY = (windowHeight |> toFloat) / 2 - (stamp.value.y - radius)
  in [ngon 5 radius |> filled color] |> collage stampSize stampSize |> clickable delete |> toForm |> rotate angle |> move (screenX, screenY)

radius = 8

-- Commands

port createStamp : Signal Stamp
port createStamp = Mouse.position |> sampleOn Mouse.clicks |> Signal.map makeCreateStamp

makeCreateStamp : (Int, Int) -> Stamp
makeCreateStamp (x, y) =
  {
    url = url,
    value = {
      x = x |> toFloat,
      y = y |> toFloat
    }
  }

port deleteStamp : Signal String
port deleteStamp = deleteStampChannel |> subscribe

deleteStampChannel : Channel String
deleteStampChannel = channel ""