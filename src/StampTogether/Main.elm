module StampTogether.Main where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Set exposing (Set)
import Json.Decode as Decode exposing (Value, Decoder, (:=))
import Json.Encode as Encode
import Graphics.Collage as Collage exposing (Form)
import Graphics.Element as Element exposing (Element)
import Graphics.Input as Input
import Text
import Color
import Dict exposing (Dict)
import Window
import Mouse
import ElmFire exposing (Reference, Snapshot, Subscription)

main =
  Signal.map2 view Window.dimensions model

-- Model

type alias Model =
  Result String (List (Remote Stamp))

model : Signal Model
model =
  Signal.map decode jsonModel

type alias Remote a =
  { a |
    url: String
  }

type alias Stamp =
  {
    x: Float,
    y: Float
  }

decode : CacheResult -> Model
decode cacheResult =
  let valueResult =
        case cacheResult of
          Unknown ->
            Err "Loading..."
          NotFound ->
            Err "Click somewhere!"
          Found value ->
            Ok value
      stampsDecoder =
        Decode.keyValuePairs stampDecoder
  in
    valueResult `Result.andThen` (\value ->
      value |> Decode.decodeValue stampsDecoder
    ) |> Result.map (\pairs ->
      pairs |> List.map (\(key, stamp) ->
        { stamp | url = rootUrl ++ "/" ++ key }
      )
    )

stampDecoder : Decoder Stamp
stampDecoder =
  Decode.object2 Stamp
    ("x" := Decode.float)
    ("y" := Decode.float)

jsonModel : Signal CacheResult
jsonModel =
  cache |> Signal.map find

find : Cache -> CacheResult
find cache =
  case cache |> Dict.get rootUrl of
    Nothing ->
      Unknown
    Just maybeValue ->
      case maybeValue of
        Nothing ->
          NotFound
        Just value ->
          Found value

type CacheResult =
  Unknown |
  NotFound |
  Found Value

cache : Signal Cache
cache =
  feed.signal |> Signal.foldp updateCache Dict.empty

type alias Cache =
  Dict String (Maybe Value)

updateCache : CacheUpdate -> Cache -> Cache
updateCache update cache =
  cache |> Dict.insert update.url update.value

type alias CacheUpdate =
  {
    url: String,
    value: Maybe Value
  }

feed : Mailbox CacheUpdate
feed =
  Signal.mailbox {
    url = "",
    value = Nothing
  }

port performObserve : Signal (Task ElmFire.Error (List ()))
port performObserve =
  Signal.map2 observe subscriptions urls

observe : Dict String Subscription -> Set String -> Task ElmFire.Error (List ())
observe subscriptions urls =
  let newUrls =
        urls |> Set.filter (\url ->
          not (subscriptions |> Dict.member url)
        ) |> Set.toList
      oldSubscriptions =
        subscriptions |> Dict.filter (\url _ ->
          not (urls |> Set.member url)
        )
      subscribes =
        newUrls |> subscribeMany
      unsubscribes =
        oldSubscriptions |> unsubscribeMany
  in (subscribes ++ unsubscribes) |> Task.sequence

subscribeMany : List String -> List (Task ElmFire.Error ())
subscribeMany urls =
  let received snapshot =
        {
          url = snapshot.reference |> ElmFire.toUrl,
          value = snapshot.value
        } |> Signal.send feed.address
      subscribeTo url =
        url |> ElmFire.fromUrl |> ElmFire.subscribe received doNothing ElmFire.valueChanged
      subscribeSuccess url subscription =
        Subscribe (Ok { url = url, subscription = subscription }) |> Signal.send subscriptionResults.address
      subscribeFailed error =
        Subscribe (Err error) |> Signal.send subscriptionResults.address
  in
    urls |> List.map (\url ->
      (subscribeTo url) `Task.andThen` (\subscription ->
        subscribeSuccess url subscription
      ) `Task.onError` (\error ->
        subscribeFailed error
      )
    )

doNothing : a -> Task x ()
doNothing =
  always (Task.succeed ())

unsubscribeMany : Dict String Subscription -> List (Task ElmFire.Error ())
unsubscribeMany subscriptions =
  let unsubscribeSuccess url =
        Unsubscribe (Ok url) |> Signal.send subscriptionResults.address
      unsubscribeFailed error =
        Unsubscribe (Err error) |> Signal.send subscriptionResults.address
  in
    subscriptions |> Dict.toList |> List.map (\(url, subscription) ->
      (subscription |> ElmFire.unsubscribe) `Task.andThen` (\_ ->
        unsubscribeSuccess url
      ) `Task.onError` (\error ->
        unsubscribeFailed error
      )
    )

subscriptionResults : Mailbox SubscriptionResult
subscriptionResults =
  Signal.mailbox Nop

type SubscriptionResult =
  Nop |
  Subscribe (Result ElmFire.Error SubscriptionTrace) |
  Unsubscribe (Result ElmFire.Error String)

type alias SubscriptionTrace =
  {
    url: String,
    subscription: Subscription
  }

subscriptions : Signal (Dict String Subscription)
subscriptions =
  subscriptionResults.signal |> Signal.foldp updateSubscriptions Dict.empty

updateSubscriptions : SubscriptionResult -> Dict String Subscription -> Dict String Subscription
updateSubscriptions subscriptionResult subscriptions =
  case subscriptionResult of
    Nop ->
      subscriptions
    Subscribe result ->
      case result of
        Ok subscribed -> subscriptions |> Dict.insert subscribed.url subscribed.subscription
        Err _ -> subscriptions
    Unsubscribe result ->
      case result of
        Ok unsubscribedUrl -> subscriptions |> Dict.remove unsubscribedUrl
        Err _ -> subscriptions

urls : Signal (Set String)
urls =
  model |> Signal.map collect

collect : Model -> Set String
collect model =
  rootUrl |> Set.singleton

rootUrl = "https://thsoft.firebaseio-demo.com/StampTogether"

-- Interface

view : (Int, Int) -> Model -> Element
view (windowWidth, windowHeight) model =
  let foreground =
        case model of
          Ok stamps ->
            stamps |> List.map (viewStamp (windowWidth, windowHeight)) |> Collage.collage windowWidth windowHeight
          Err error ->
            error |> Text.fromString |> Element.leftAligned
      messageCreate =
        Signal.message createStampCommands.address ()
      background =
        Element.spacer windowWidth windowHeight |> Input.clickable messageCreate
  in [background, foreground] |> Element.layers

createStampCommands : Mailbox ()
createStampCommands =
  Signal.mailbox ()

port performCreateStamp : Signal (Task ElmFire.Error ())
port performCreateStamp =
  Mouse.position |> Signal.sampleOn createStampCommands.signal |> Signal.map createStamp

createStamp : (Int, Int) -> Task ElmFire.Error ()
createStamp (x, y) =
  if (x == 0) && (y == 0) then
    doNothing ()
  else
    let stampValue =
          Encode.object [
            ("x", Encode.float (x |> toFloat)),
            ("y", Encode.float (y |> toFloat))
          ]
        location = rootUrl |> ElmFire.fromUrl |> ElmFire.push
    in location |> ElmFire.set stampValue |> Task.map (always ())

viewStamp : (Int, Int) -> Remote Stamp -> Form
viewStamp (windowWidth, windowHeight) stamp =
  let angle = (stamp.x * stamp.y) |> degrees
      color = Color.hsla angle 1 0.5 1
      radius = 8
      stampSize = radius * 2
      screenX = stamp.x - (windowWidth |> toFloat) / 2
      screenY = (windowHeight |> toFloat) / 2 - stamp.y
      element = [Collage.ngon 5 radius |> Collage.filled color |> Collage.rotate angle] |> Collage.collage stampSize stampSize
      messageDelete = Signal.message deleteStampCommands.address stamp.url
  in element |> Input.clickable messageDelete |> Collage.toForm |> Collage.move (screenX, screenY)

deleteStampCommands : Mailbox String
deleteStampCommands =
  Signal.mailbox ""

port performDeleteStamp : Signal (Task ElmFire.Error ())
port performDeleteStamp =
  deleteStampCommands.signal |> Signal.map deleteStamp

deleteStamp : String -> Task ElmFire.Error ()
deleteStamp url =
  if url == "" then
    doNothing ()
  else
    url |> ElmFire.fromUrl |> ElmFire.remove |> Task.map (always ())
