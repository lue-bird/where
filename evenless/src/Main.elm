port module Main exposing (main)

{-| time to face gravity.
-}

import Audio exposing (Audio, AudioCmd, AudioData)
import Browser
import Browser.Dom
import Browser.Events
import Collage exposing (Collage)
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color exposing (Color, rgb, rgba)
import Color.Manipulate as Color
import Element as Ui
import Element.Background as Background
import Element.Border as UiBorder
import Element.Events as Ui
import Element.Font as Font
import Element.Input as UiInput
import Html
import Html.Attributes
import Json.Decode
import Json.Encode
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List exposing (tail)
import List.Extra as List
import List.NonEmpty exposing (NonEmpty)
import Random
import Task
import Time exposing (Posix)
import Xy exposing (Xy, length, x, xy, y)


main : Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.documentWithAudio
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        , view = viewDocument
        , audio = audio
        }


type alias Model =
    { windowSize : Xy Float
    , pressedKeys : List Key
    , gameStage : GameStage
    , framesPlayed : Int -- frames
    , music : Maybe (Result Audio.LoadError Audio.Source)
    , lines : NonEmpty (List (Xy Float))
    , playerPosition : Xy Float
    , playerVelocity : Xy Float
    , mouseUpOrDown : MouseUpOrDown
    }


type MouseUpOrDown
    = MouseIsUp
    | MouseIsDown


type GameStage
    = Playing
    | GameOver


mapV : (v -> v) -> { r | v : v } -> { r | v : v }
mapV map r =
    { r | v = map r.v }


mapPosition :
    (pos -> pos)
    -> { r | position : pos }
    -> { r | position : pos }
mapPosition map r =
    { r | position = map r.position }


distance : Xy Float -> Xy Float -> Float
distance a b =
    difference a b |> length


init : ( Model, Cmd Msg, AudioCmd Msg )
init =
    newGame
        (Audio.loadAudio SoundLoadingResult
            "https://lue-bird.github.io/time-to-face-gravity/"
        )


newGame : AudioCmd Msg -> ( Model, Cmd Msg, AudioCmd Msg )
newGame audioCmd =
    ( { windowSize = Xy.zero
      , pressedKeys = []
      , lines =
            List.NonEmpty.singleton
                (List.range -20 20
                    |> List.map (\x -> ( toFloat x * playerRadius, -40 ))
                )
      , gameStage = Playing
      , framesPlayed = 0
      , music = Nothing
      , playerPosition = Xy.zero
      , playerVelocity = Xy.zero
      , mouseUpOrDown = MouseIsUp
      }
    , Browser.Dom.getViewport
        |> Task.perform
            (.viewport >> Xy.fromSize >> Resized)
    , audioCmd
    )


type Msg
    = NewGameClicked
    | Resized (Xy Float)
    | Frame Int
    | MouseMove (Xy Float)
    | MouseUp (Xy Float)
    | MouseDown (Xy Float)
    | KeyMsg Keyboard.Msg
    | SoundLoadingResult (Result Audio.LoadError Audio.Source)


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg =
    case msg of
        NewGameClicked ->
            \_ -> newGame Audio.cmdNone

        Resized windowSize ->
            \m ->
                ( { m
                    | windowSize =
                        windowSize
                            |> Xy.map (\c -> c - 3.5)
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        Frame _ ->
            \model ->
                ( let
                    arrowMovedVelocity =
                        model.playerVelocity
                            |> Xy.map2 (+)
                                (Keyboard.Arrows.arrows model.pressedKeys
                                    |> Xy.fromXY
                                    |> Xy.map (toFloat >> (*) 0.6)
                                )

                    slowerVelocity =
                        arrowMovedVelocity
                            |> Xy.map ((*) 0.98)

                    newPosition =
                        model.playerPosition
                            |> Xy.map2 (+) model.playerVelocity

                    collisionWithLines =
                        model.lines
                            |> List.NonEmpty.toList
                            |> List.map pointsToLines
                            |> List.map
                                (List.filterMap
                                    (circleLineCollision
                                        { radius = playerRadius
                                        , velocity = slowerVelocity
                                        , position = newPosition
                                        }
                                    )
                                )
                            |> List.concat
                  in
                  case collisionWithLines of
                    [] ->
                        { model
                            | playerPosition = newPosition
                            , playerVelocity =
                                slowerVelocity
                                    |> Xy.mapY (\y -> y - 1)
                                    |> lengthAtMost 10
                        }

                    { velocity } :: _ ->
                        { model
                            | playerPosition =
                                model.playerPosition
                                    |> Xy.map2 (+) velocity
                            , playerVelocity = velocity
                        }
                , Cmd.none
                , Audio.cmdNone
                )

        MouseMove mousePosition ->
            \model ->
                ( case model.mouseUpOrDown of
                    MouseIsDown ->
                        let
                            ( head, tail ) =
                                model.lines

                            linePointPosition =
                                mousePosition
                                    |> Xy.map2 (\window mouse -> mouse - window)
                                        (model.windowSize |> Xy.map (\c -> c / 2))
                                    |> Xy.mapY (\y -> -y)
                                    |> Xy.map2 (+) model.playerPosition
                        in
                        { model
                            | lines =
                                List.NonEmpty.fromCons
                                    (head |> (::) linePointPosition)
                                    tail
                        }

                    MouseIsUp ->
                        model
                , Cmd.none
                , Audio.cmdNone
                )

        MouseUp mousePosition ->
            \model ->
                ( { model
                    | lines =
                        let
                            ( head, tail ) =
                                model.lines
                        in
                        ( head |> (::) mousePosition, tail )
                    , mouseUpOrDown = MouseIsUp
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        MouseDown mousePosition ->
            \model ->
                ( { model
                    | lines =
                        model.lines
                            |> List.NonEmpty.cons [ mousePosition ]
                    , mouseUpOrDown = MouseIsDown
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        KeyMsg keyMsg ->
            \model ->
                ( { model
                    | pressedKeys =
                        Keyboard.update keyMsg model.pressedKeys
                  }
                , Cmd.none
                , Audio.cmdNone
                )

        SoundLoadingResult result ->
            \m ->
                ( { m | music = Just result }
                , Cmd.none
                , Audio.cmdNone
                )



-- The ports must have these specific names.


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    [ Browser.Events.onResize
        (\w h ->
            Resized (( w, h ) |> Xy.map toFloat)
        )
    , Browser.Events.onAnimationFrame
        (Time.posixToMillis >> Frame)
    , Sub.map KeyMsg Keyboard.subscriptions
    ]
        |> Sub.batch


playerRadius : Float
playerRadius =
    20


viewDocument : AudioData -> Model -> Browser.Document Msg
viewDocument _ model =
    { title = "evenless"
    , body =
        let
            viewPlaying =
                view model
                    |> Collage.Render.svgBox model.windowSize
                    |> Ui.html

            content =
                case model.gameStage of
                    Playing ->
                        viewPlaying

                    GameOver ->
                        viewPlaying
                            |> Ui.el
                                [ Ui.inFront
                                    (viewGameOver model
                                        |> Ui.el
                                            [ Ui.width Ui.fill
                                            , Ui.height Ui.fill
                                            , Background.color (Ui.rgba 0 0 0 0.9)
                                            ]
                                    )
                                ]
        in
        content
            |> Ui.layout
                [ Html.Attributes.style "overflow" "hidden"
                    |> Ui.htmlAttribute
                , Ui.height Ui.fill
                , Ui.width Ui.fill
                , Background.color (Ui.rgb 0 0 0)
                ]
            |> List.singleton
    }


viewGameOver : { a | framesPlayed : Int } -> Ui.Element Msg
viewGameOver { framesPlayed } =
    [ Ui.text
        ("score " ++ (framesPlayed // 24 |> String.fromInt))
        |> Ui.el
            [ Font.size 30
            , Ui.alignBottom
            ]
    , UiInput.button
        [ Font.size 33
        , Background.color (Ui.rgba 0 1 1 0.1)
        , UiBorder.rounded 100
        , Ui.padding 20
        ]
        { label = Ui.text "New game"
        , onPress = Just NewGameClicked
        }
    ]
        |> List.map (Ui.el [ Ui.centerX ])
        |> Ui.column
            [ Font.color (Ui.rgb 1 1 1)
            , Ui.centerX
            , Ui.centerY
            , Ui.spacing 10
            ]


view :
    { a
        | windowSize : Xy Float
        , lines : NonEmpty (List (Xy Float))
        , playerPosition : Xy Float
    }
    -> Collage Msg
view { windowSize, lines, playerPosition } =
    let
        viewPlayer =
            Collage.ellipse playerRadius playerRadius
                |> Collage.filled
                    (Color.rgb 1 0 0 |> Collage.uniform)

        viewLine line =
            Collage.path line
                |> Collage.traced
                    (Collage.solid 2.3
                        (Color.rgb 1 1 1 |> Collage.uniform)
                    )
    in
    [ [ viewPlayer
      , lines
            |> List.NonEmpty.toList
            |> List.map viewLine
            |> Collage.group
            |> Collage.shift
                (playerPosition |> Xy.map (\c -> -c))
      ]
        |> Collage.group
    , Collage.rectangle (x windowSize) (y windowSize)
        |> Collage.filled
            (Collage.uniform (rgb 0 0 0))
    ]
        |> Collage.group
        |> Collage.Events.onMouseMove MouseMove
        |> Collage.Events.onMouseUp MouseUp
        |> Collage.Events.onMouseDown MouseDown


audio : AudioData -> Model -> Audio
audio _ { framesPlayed, music } =
    case music of
        Just (Ok source) ->
            Audio.audio source
                (Time.millisToPosix framesPlayed)

        Nothing ->
            Audio.silence

        Just (Err error) ->
            Debug.todo (Debug.toString error)



-- util


randomAndMap :
    Random.Generator a
    -> Random.Generator (a -> b)
    -> Random.Generator b
randomAndMap randomAspect =
    Random.map2 (\a f -> f a) randomAspect


difference : Xy number -> Xy number -> Xy number
difference =
    Xy.map2 (\aC bC -> bC - aC)


lengthAtMost : Float -> Xy Float -> Xy Float
lengthAtMost maximumLength xy =
    let
        length_ =
            length xy
    in
    if length_ > maximumLength then
        xy |> Xy.map ((*) (maximumLength / length_))

    else
        xy


type Line
    = Line (Xy Float) (Xy Float)



-- collision stuff from https://ericleong.me/research/circle-line/


pointsToLines : List (Xy Float) -> List Line
pointsToLines points =
    List.map2 Line
        points
        (points |> List.drop 1)


linesCollide : Line -> Line -> Maybe (Xy Float)
linesCollide a b =
    let
        (Line ( aX0, aY0 ) ( aXEnd, aYEnd )) =
            a

        (Line ( bX0, bY0 ) ( bXEnd, bYEnd )) =
            b

        a0 =
            aYEnd - aY0

        b0 =
            aX0 - aXEnd

        c0 =
            a0 * aX0 + b0 * aY0

        a1 =
            bYEnd - bY0

        b1 =
            bX0 - bXEnd

        c1 =
            a1 * bX0 + b1 * bY0

        det =
            a0 * b1 - a1 * b0
    in
    if det /= 0 then
        let
            x =
                (b1 * c0 - b0 * c1) / det

            y =
                (a0 * c1 - a1 * c0) / det
        in
        if
            (x >= min aX0 aXEnd)
                && (x <= max aX0 aXEnd)
                && (x >= min bX0 bXEnd)
                && (x <= max bX0 bXEnd)
                && (y >= min aY0 aYEnd)
                && (y <= max aY0 aYEnd)
                && (y >= min bY0 bYEnd)
                && (y <= max bY0 bYEnd)
        then
            Just ( x, y )

        else
            Nothing

    else
        Nothing


closestPointOnLine : Line -> Xy Float -> Xy Float
closestPointOnLine line point =
    let
        (Line ( lX0, lY0 ) ( lXEnd, lYEnd )) =
            line

        ( x, y ) =
            point

        a1 =
            lYEnd - lY0

        b1 =
            lX0 - lXEnd

        c1 =
            (lYEnd - lY0) * lX0 + (lX0 - lXEnd) * lY0

        c2 =
            -b1 * x + a1 * y

        det =
            a1 * a1 - -b1 * b1
    in
    if det /= 0 then
        ( (a1 * c1 - b1 * c2) / det
        , (a1 * c2 - -b1 * c1) / det
        )

    else
        point


circleLineCollision :
    { radius : Float, position : Xy Float, velocity : Xy Float }
    -> Line
    -> Maybe { velocity : Xy Float }
circleLineCollision { radius, position, velocity } line =
    let
        closestOnLine =
            closestPointOnLine line position

        differenceToLine =
            difference closestOnLine position
    in
    if Xy.length differenceToLine < radius then
        let
            overlapVector =
                Xy.map2 (-)
                    differenceToLine
                    (Xy.normalize differenceToLine
                        |> Xy.map ((*) radius)
                    )
        in
        Just
            { velocity =
                overlapVector
                    |> Xy.map ((*) (length velocity))
            }

    else
        Nothing
