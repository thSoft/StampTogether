module StampTogether where

import Signal (..)
import Signal
import Time (..)
import List (..)
import List
import Maybe (..)
import Maybe
import Color (..)
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Text
import Window
import Mouse
import DragAndDrop (..)

main : Signal Element
main = Signal.map4 view Window.dimensions lastDraggedStamp dragging stamps

-- Model

port stamps : Signal (List Stamp)

type alias Stamp = {
  url: String,
  value: Position
}

type alias Position = {
  x: Float,
  y: Float
}

lastDraggedStamp : Signal (Maybe (Dragged Stamp))
lastDraggedStamp = dragEvents |> foldp updateLastDraggedStamp Nothing

type alias Dragged a = { a |
  moved: Bool
}

dragging : Signal Bool
dragging = dragEvents |> foldp updateDragging False

-- View

view : (Int, Int) -> Maybe (Dragged Stamp) -> Bool -> List Stamp -> Element
view (windowWidth, windowHeight) lastDraggedStampValue draggingValue stampsValue =
  let viewStamp stamp =
        let actualStamp =
              case lastDraggedStampValue of
                Nothing -> stamp
                Just draggedStamp -> if draggingValue && (draggedStamp.url == stamp.url) then { draggedStamp - moved } else stamp
            isDragged =
              case lastDraggedStampValue of
                Nothing -> False
                Just { url } -> draggingValue && (url == stamp.url)
            angle = (actualStamp.value.x * actualStamp.value.y) |> degrees
            color = hsla angle 1 0.5 (if isDragged then 0.5 else 1)
            radius = 8
            forms = [ngon 5 radius |> filled color]
            stampSize = radius * 2
            hover mouseOver = (if mouseOver then Just actualStamp else Nothing) |> send hoverStampChannel
            screenX = actualStamp.value.x - (windowWidth |> toFloat) / 2
            screenY = (windowHeight |> toFloat) / 2 - actualStamp.value.y
        in forms |> collage stampSize stampSize |> hoverable hover |> toForm |> rotate angle |> move (screenX, screenY)
      foreground = stampsValue |> List.map viewStamp |> collage windowWidth windowHeight
      create = () |> send createStampChannel
      background = spacer windowWidth windowHeight |> clickable create
  in [background, foreground] |> layers

-- Update

port observedUrls : Signal (List String)
port observedUrls =
  let count _ index = index + 1
      compute index = if index == 0 then [] else [modelUrl]
  in every (0.1 * second) |> foldp count 0 |> Signal.map compute |> dropRepeats

modelUrl = "https://thsoft.firebaseio-demo.com/StampTogether"

dragEvents : Signal (Maybe (Stamp, Action))
dragEvents = trackMany Nothing (hoverStampChannel |> subscribe)

hoverStampChannel : Channel (Maybe Stamp)
hoverStampChannel = channel Nothing

updateLastDraggedStamp : Maybe (Stamp, Action) -> Maybe (Dragged Stamp) -> Maybe (Dragged Stamp)
updateLastDraggedStamp dragEventsValue lastDraggedStampValue =
  case dragEventsValue of
    Nothing -> lastDraggedStampValue
    Just (stamp, action) ->
      case action of
        Lift -> Just { stamp | moved = False }
        MoveBy delta -> lastDraggedStampValue |> Maybe.map (moveStampBy delta)
        Release -> lastDraggedStampValue

moveStampBy : (Int, Int) -> (Dragged Stamp) -> (Dragged Stamp)
moveStampBy (dx, dy) stamp = 
  let newPosition =
        {
          x = stamp.value.x + (dx |> toFloat),
          y = stamp.value.y + (dy |> toFloat)
        }
  in
    { stamp |
      value <- newPosition,
      moved <- True
    }

updateDragging : Maybe (Stamp, Action) -> Bool -> Bool
updateDragging dragEventsValue draggingValue =
  case dragEventsValue of
    Nothing -> draggingValue
    Just (stamp, action) ->
      case action of
        Lift -> True
        MoveBy _ -> draggingValue
        Release -> False

-- Commands

port createStamp : Signal Stamp
port createStamp = Mouse.position |> sampleOn (createStampChannel |> subscribe) |> Signal.map makeCreateStamp

createStampChannel : Channel ()
createStampChannel = channel ()

makeCreateStamp : (Int, Int) -> Stamp
makeCreateStamp (x, y) =
  {
    url = modelUrl,
    value = {
      x = x |> toFloat,
      y = y |> toFloat
    }
  }

port deleteStamp : Signal (Maybe String)
port deleteStamp = droppedStamp |> dropIf stampMoved Nothing |> Signal.map (Maybe.map .url)

droppedStamp : Signal (Maybe (Dragged Stamp))
droppedStamp =
  let isRelease dragEventsValue =
        case dragEventsValue of
          Just (_, Release) -> True
          _ -> False
      drop = dragEvents |> keepIf isRelease Nothing
  in lastDraggedStamp |> sampleOn drop

stampMoved : Maybe (Dragged Stamp) -> Bool
stampMoved lastDraggedStampValue =
  case lastDraggedStampValue of
    Nothing -> False
    Just draggedStamp -> draggedStamp.moved

port moveStamp : Signal (Maybe Stamp)
port moveStamp = droppedStamp |> keepIf stampMoved Nothing |> Signal.map (Maybe.map makeMoveStamp)

makeMoveStamp : Dragged Stamp -> Stamp
makeMoveStamp draggedStamp = { draggedStamp - moved }