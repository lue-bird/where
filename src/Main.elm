port module Main exposing (Event, main)

{-| GMTK Game Jam 2022

  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-geometry/latest>
  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/>
  - <https://dark.elm.dmy.fr/packages/elm-community/typed-svg/latest/>
  - <https://dark.elm.dmy.fr/packages/Spaxe/svg-pathd/latest/Svg-PathD>
  - <https://dark.elm.dmy.fr/packages/Herteby/simplex-noise/latest/>
  - <https://www.youtube.com/watch?v=CoMHkeAXEUY>
  - elm-optimize-level-2 src/Main.elm --optimize-speed
      - <https://github.com/mdgriffith/elm-optimize-level-2>

-}

import Angle
import AppStep exposing (AppStep)
import Audio exposing (Audio, AudioData, elementWithAudio)
import Browser.Dom
import Browser.Events as Browser
import Color exposing (rgb, rgba)
import Conversation
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes as Html
import Json.Decode
import Json.Encode
import Keyboard exposing (Key, KeyChange(..))
import Keyboard.Arrows
import Linear exposing (DirectionLinear(..))
import List.Extra
import Pixels exposing (Pixels)
import Process
import Quantity exposing (Quantity)
import Random
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Scroll exposing (FocusGap, Scroll)
import Simplex
import Stack
import Svg.PathD
import Task
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Events as Svg
import TypedSvg.Filters as SvgFilter
import TypedSvg.Filters.Attributes as SvgFilterA
import TypedSvg.Types as Svg
import Where.Conversation


main : Program () (Audio.Model Event State) (Audio.Msg Event)
main =
    elementWithAudio
        { init = init >> AppStep.toTuple
        , subscriptions = subscriptions
        , update =
            \audioData event deprecatedState ->
                deprecatedState
                    |> reactTo event audioData
                    |> AppStep.toTuple
        , view = htmlUi
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }


type alias State =
    RecordWithoutConstructorFunction
        { seed : Random.Seed
        , permutations : Simplex.PermutationTable
        , windowWidth : Quantity Float Pixels
        , windowHeight : Quantity Float Pixels
        , keysPressed : List Key
        , keyUpChanges : List Key
        , keyDownChanges : List Key
        , time : Duration
        , notifySound : Result (ProcessError Audio.LoadError) Audio.Source

        --
        , conversationHistory :
            List
                { speaker : Where.Conversation.Speaker
                , lines : Where.Conversation.Lines
                }
        , conversationStep :
            Where.Conversation.Step
        , conversationPossibilityWatched :
            Int
        }


type ProcessError loadError
    = Loading
    | LoadError loadError


type Event
    = WindowSized
        { width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    | KeyEvent Keyboard.Msg
    | ConversationChoicePossibilityWatched Int
    | ConversationPredeterminedPossibilityWatched
        { speaker : Where.Conversation.Speaker
        , state : Where.Conversation.State
        , possibilities : Scroll Where.Conversation.Lines FocusGap Never
        , fullWatchDuration : Duration
        }
    | ConversationPossibilityClicked
        { state : Where.Conversation.State
        , speaker : Where.Conversation.Speaker
        , lines : Where.Conversation.Lines
        }
    | DigSoundLoaded (Result Audio.LoadError Audio.Source)
    | AnimationFramePassed { delta : Duration }
    | PermutationsGenerated Simplex.PermutationTable


init : () -> AppStep State Event
init () =
    let
        nextStep : Where.Conversation.Step
        nextStep =
            Where.Conversation.Start
                |> Conversation.step Where.Conversation.conversation
    in
    (case nextStep of
        Conversation.Choice _ ->
            AppStep.to

        Conversation.Predetermined predetermined ->
            stateWatchPredeterminedPossibilities
                { speaker = predetermined.speaker
                , possibilities = predetermined.possibilities
                , state = predetermined.state
                , fullWatchDuration = Duration.seconds 0
                }
    )
        { seed =
            -- dummy. Will be replaced
            Random.initialSeed 1329952631
        , permutations =
            -- dummy. Will be replaced
            Simplex.permutationTableFromInt 1329952631
        , windowWidth = Pixels.pixels 1920
        , windowHeight = Pixels.pixels 1080
        , keysPressed = []
        , keyUpChanges = []
        , keyDownChanges = []
        , time = Duration.seconds 0
        , notifySound = Err Loading

        --
        , conversationHistory = []
        , conversationStep = nextStep
        , conversationPossibilityWatched = 0
        }
        |> AppStep.commandsAdd
            [ Browser.Dom.getViewport
                |> Task.perform
                    (\{ viewport } ->
                        WindowSized
                            { width = Pixels.pixels viewport.width
                            , height = Pixels.pixels viewport.height
                            }
                    )
            ]
        |> AppStep.audioCommandsAdd
            [ Audio.loadAudio
                (\result -> DigSoundLoaded result)
                "https://cors-anywhere.herokuapp.com/https://freepd.com/music/Wakka%20Wakka.mp3"
            ]


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : AudioData -> State -> Sub Event
subscriptions _ =
    \state ->
        [ Browser.onAnimationFrameDelta
            (\deltaMilliseconds ->
                AnimationFramePassed
                    { delta = Duration.milliseconds deltaMilliseconds }
            )
        , Browser.onResize
            (\width height ->
                WindowSized
                    { width = Pixels.pixels (width |> toFloat)
                    , height = Pixels.pixels (height |> toFloat)
                    }
            )
        , Keyboard.subscriptions
            |> Sub.map
                (\keyBoardEvent ->
                    KeyEvent keyBoardEvent
                )
        ]
            |> Sub.batch


reactTo : Event -> AudioData -> State -> AppStep State Event
reactTo event _ =
    case event of
        WindowSized size ->
            \state ->
                AppStep.to
                    { state
                        | windowWidth = size.width |> Quantity.plus (Pixels.float 16)
                        , windowHeight = size.height |> Quantity.plus (Pixels.float 16)
                    }
                    |> AppStep.commandsAdd
                        [ Simplex.permutationTableGenerator
                            |> Random.generate PermutationsGenerated
                        ]

        KeyEvent keyEvent ->
            \state ->
                let
                    ( keysPressedUpdated, maybeKeyChange ) =
                        state.keysPressed
                            |> Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyEvent

                    possibilitiesMaybe =
                        case state.conversationStep of
                            Conversation.Predetermined predetermined ->
                                { count =
                                    predetermined.possibilities |> Scroll.length
                                , selected =
                                    { speaker = predetermined.speaker
                                    , lines = predetermined.possibilities |> Scroll.focusItem
                                    , state = predetermined.state
                                    }
                                }
                                    |> Just

                            Conversation.Choice [ choice ] ->
                                { count =
                                    choice.possibilities |> List.length
                                , selected =
                                    let
                                        selectedPossibility =
                                            choice.possibilities
                                                |> List.Extra.getAt state.conversationPossibilityWatched
                                    in
                                    { speaker = choice.speaker
                                    , lines =
                                        case selectedPossibility of
                                            Just exists ->
                                                exists.lines

                                            Nothing ->
                                                []
                                    , state =
                                        case selectedPossibility of
                                            Just exists ->
                                                exists.state

                                            Nothing ->
                                                Where.Conversation.Start
                                    }
                                }
                                    |> Just

                            Conversation.Choice (_ :: _ :: _) ->
                                Nothing

                            Conversation.Choice [] ->
                                Nothing

                    keyChangesUpdated =
                        case possibilitiesMaybe of
                            Nothing ->
                                AppStep.to state

                            Just possibilities ->
                                let
                                    watch indexChange =
                                        AppStep.to
                                            { state
                                                | conversationPossibilityWatched =
                                                    state.conversationPossibilityWatched
                                                        |> indexChange
                                                        |> modBy possibilities.count
                                            }
                                in
                                case maybeKeyChange of
                                    Just (KeyUp keyChange) ->
                                        case keyChange of
                                            Keyboard.ArrowDown ->
                                                watch (\x -> x - 1)

                                            Keyboard.Character "s" ->
                                                watch (\x -> x - 1)

                                            Keyboard.ArrowUp ->
                                                watch (\x -> x + 1)

                                            Keyboard.Character "w" ->
                                                watch (\x -> x + 1)

                                            Keyboard.Enter ->
                                                state
                                                    |> stateConversationStep possibilities.selected

                                            _ ->
                                                AppStep.to state

                                    Just (KeyDown _) ->
                                        AppStep.to state

                                    Nothing ->
                                        AppStep.to state
                in
                keyChangesUpdated
                    |> AppStep.map
                        (\r ->
                            { r
                                | keysPressed = keysPressedUpdated
                            }
                        )

        ConversationChoicePossibilityWatched index ->
            \state ->
                AppStep.to
                    { state
                        | conversationPossibilityWatched = index
                    }

        ConversationPredeterminedPossibilityWatched predetermined ->
            let
                possibilityCount =
                    predetermined.possibilities |> Scroll.length
            in
            if
                predetermined.fullWatchDuration
                    |> Quantity.greaterThan
                        (Duration.seconds
                            (1.2
                                + 0.7
                                * (predetermined.possibilities
                                    |> Scroll.length
                                    |> toFloat
                                  )
                            )
                        )
            then
                \state ->
                    AppStep.to
                        { state
                            | conversationPossibilityWatched =
                                predetermined.possibilities |> Scroll.side Down |> Stack.length
                        }
                        |> AppStep.commandsAdd
                            [ Process.sleep 700
                                |> Task.perform
                                    (\() ->
                                        ConversationPossibilityClicked
                                            { state = predetermined.state
                                            , speaker = predetermined.speaker
                                            , lines = predetermined.possibilities |> Scroll.focusItem
                                            }
                                    )
                            ]

            else
                stateWatchPredeterminedPossibilities predetermined

        ConversationPossibilityClicked possibility ->
            \state ->
                state
                    |> stateConversationStep possibility

        PermutationsGenerated permutations ->
            \state ->
                AppStep.to
                    { state
                        | permutations = permutations
                    }

        DigSoundLoaded digSound ->
            \state ->
                AppStep.to
                    { state
                        | notifySound =
                            digSound
                                |> Result.mapError LoadError
                    }

        AnimationFramePassed { delta } ->
            \state ->
                AppStep.to
                    { state
                        | time = state.time |> Quantity.plus delta
                    }


stateConversationStep possibility =
    let
        nextStep : Where.Conversation.Step
        nextStep =
            possibility.state
                |> Conversation.step Where.Conversation.conversation
    in
    \state ->
        { state
            | conversationStep = nextStep
            , conversationHistory =
                state.conversationHistory
                    |> (::)
                        { speaker = possibility.speaker
                        , lines = possibility.lines
                        }
        }
            |> (case nextStep of
                    Conversation.Choice _ ->
                        AppStep.to

                    Conversation.Predetermined predetermined ->
                        stateWatchPredeterminedPossibilities
                            { speaker = predetermined.speaker
                            , possibilities = predetermined.possibilities
                            , state = predetermined.state
                            , fullWatchDuration = Duration.seconds 0
                            }
               )


stateWatchPredeterminedPossibilities predetermined =
    \state ->
        AppStep.to state
            |> (let
                    ( random, seedNew ) =
                        state.seed
                            |> Random.step
                                (Random.map2
                                    (\index watchDuration ->
                                        { watchDuration = watchDuration, index = index }
                                    )
                                    (Random.int 0 ((predetermined.possibilities |> Scroll.length) - 1))
                                    (Random.map
                                        (Duration.milliseconds << toFloat)
                                        (Random.int 120 760)
                                    )
                                )
                in
                AppStep.map
                    (\r ->
                        { r
                            | conversationPossibilityWatched = random.index
                            , seed = seedNew
                        }
                    )
                    >> AppStep.commandsAdd
                        [ Process.sleep (random.watchDuration |> Duration.inMilliseconds)
                            |> Task.perform
                                (\() ->
                                    ConversationPredeterminedPossibilityWatched
                                        { speaker = predetermined.speaker
                                        , possibilities = predetermined.possibilities
                                        , state = predetermined.state
                                        , fullWatchDuration =
                                            predetermined.fullWatchDuration
                                                |> Quantity.plus random.watchDuration
                                        }
                                )
                        ]
               )


noise :
    { permutations : Simplex.PermutationTable
    , x : Float
    , y : Float
    }
    -> Float
noise { permutations, x, y } =
    let
        scaling =
            0.5

        negToPos1 =
            Simplex.fractal2d
                { scale = 4.3 * scaling
                , steps = (6 * scaling) |> round
                , stepSize = 4.2 * scaling
                , persistence = 2.2 * scaling
                }
                permutations
                x
                y
    in
    (negToPos1 + 1) / 2



-- ui


htmlUi : AudioData -> State -> Html Event
htmlUi _ =
    \state ->
        state
            |> ui
            |> List.singleton
            |> svg
                [ SvgA.viewBox
                    0
                    0
                    (state.windowWidth |> Pixels.toFloat)
                    ((state.windowHeight |> Pixels.toFloat) - 12)
                , Html.style "width" "100%"
                ]


background : Svg event_
background =
    Svg.rect
        [ SvgA.fill (Svg.Reference "background")
        , SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 100)
        ]
        []


svgDefinitions : Svg event_
svgDefinitions =
    Svg.defs
        []
        [ Svg.radialGradient
            [ SvgA.id "background"
            , SvgA.r (Svg.percent 150)
            ]
            [ Svg.stop
                [ SvgA.offset "37%"
                , SvgA.stopColor (rgb 0 0.1 0.1 |> Color.toCssString)
                ]
                []
            , Svg.stop
                [ SvgA.offset "100%"
                , SvgA.stopColor (rgba 0 0 0 0.4 |> Color.toCssString)
                ]
                []
            ]
        , SvgFilter.gaussianBlur
            [ SvgA.id "blur"
            , SvgA.x (Svg.px 0)
            , SvgA.y (Svg.px 0)
            , SvgFilterA.in_ Svg.InSourceGraphic
            , SvgA.stdDeviation "15"
            ]
            []
        ]


bars : Svg event_
bars =
    [ Svg.rect
        [ SvgA.fill (Svg.Paint Color.black)
        , SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 6)
        ]
        []
    , Svg.rect
        [ SvgA.fill (Svg.Paint Color.black)
        , SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 8)
        , SvgA.y (Svg.percent 94)
        ]
        []
    ]
        |> Svg.g
            []


ui : State -> Svg Event
ui =
    \state ->
        [ svgDefinitions
        , background
        , [ circles { time = state.time }
          , detailCircles { time = state.time }
          , conversationUi
                { windowWidth = state.windowWidth |> Pixels.toFloat
                , windowHeight = state.windowHeight |> Pixels.toFloat
                , conversationStep = state.conversationStep
                , conversationHistory = state.conversationHistory
                , conversationPossibilityWatched = state.conversationPossibilityWatched
                }
          ]
            |> Svg.g
                [ SvgA.transform
                    [ Svg.Translate
                        ((state.windowWidth |> Pixels.toFloat) / 2)
                        ((state.windowHeight |> Pixels.toFloat) / 2)
                    ]
                ]
        , bars
        ]
            |> Svg.g
                ([]
                    ++ (case state.conversationStep of
                            Conversation.Choice _ ->
                                [ SvgA.cursor Svg.CursorCrosshair ]

                            Conversation.Predetermined _ ->
                                [ SvgA.cursor Svg.CursorWait
                                ]
                       )
                )


circles : { time : Duration } -> Svg event_
circles { time } =
    let
        grow =
            (time |> Duration.inSeconds)
                |> pulseOver 30
    in
    List.range 3 14
        |> List.map toFloat
        |> List.map
            (\size ->
                Svg.circle
                    [ SvgA.r (Svg.percent ((size / 14) * 36 + grow ^ 3))
                    , SvgA.strokeWidth (Svg.px ((size / 14) * 40))
                    , SvgA.stroke (Svg.Paint (rgba 0 0 0 0.14))
                    , SvgA.fill (Svg.Paint (rgba 0 0 0 0))
                    ]
                    []
            )
        |> Svg.g []


{-| Cycles sin between 0 and 1
-}
pulseOver length current =
    sin (turns (current / length))


detailCircles : { time : Duration } -> Svg event_
detailCircles { time } =
    let
        float =
            (time |> Duration.inSeconds)
                |> pulseOver 40
    in
    List.range 3 14
        |> List.map toFloat
        |> List.map
            (\size ->
                Svg.circle
                    [ SvgA.r (Svg.percent ((size / 14) * 15))
                    , SvgA.strokeWidth (Svg.px ((size / 14) * 12))
                    , SvgA.stroke (Svg.Paint (rgba 0 0 0 (0.2 + float * 0.03)))
                    , SvgA.fill (Svg.Paint (rgba 0 0 0 0))
                    , SvgA.cx (Svg.percent ((size / 14) * 30))
                    ]
                    []
                    |> List.singleton
                    |> Svg.g
                        [ SvgA.transform
                            [ Svg.Rotate (Angle.turns (size / 5 + float * 0.06) |> Angle.inDegrees) 0 0 ]
                        ]
            )
        |> Svg.g []


conversationUi :
    { windowWidth : Float
    , windowHeight : Float
    , conversationStep : Where.Conversation.Step
    , conversationHistory :
        List
            { speaker : Where.Conversation.Speaker
            , lines : Where.Conversation.Lines
            }
    , conversationPossibilityWatched : Int
    }
    -> Svg Event
conversationUi { windowWidth, windowHeight, conversationStep, conversationHistory, conversationPossibilityWatched } =
    [ conversationHistory
        |> conversationHistoryUi
        |> List.singleton
        |> Svg.g
            [ SvgA.transform [ Svg.Translate 0 (windowHeight * 0.05) ]
            ]
    , { step = conversationStep
      , possibilityWatched = conversationPossibilityWatched
      }
        |> conversationStepUi
        |> List.singleton
        |> Svg.g
            [ SvgA.transform [ Svg.Translate 0 (windowHeight * 0.18) ]
            ]
    ]
        |> Svg.g
            [ SvgA.transform [ Svg.Translate -(windowWidth * 0.42) 0 ]
            ]


conversationStepUi :
    { step : Where.Conversation.Step
    , possibilityWatched : Int
    }
    -> Svg Event
conversationStepUi =
    \conversation ->
        case conversation.step of
            Conversation.Predetermined predetermined ->
                [ predetermined.speaker |> speakerUi
                , predetermined.possibilities
                    |> Scroll.toList
                    |> List.indexedMap
                        (\index possibility ->
                            possibility
                                |> possibilityUi
                                    (if index == conversation.possibilityWatched then
                                        [ SvgA.fontWeight Svg.FontWeightBolder
                                        ]

                                     else
                                        []
                                    )
                                |> List.singleton
                                |> Svg.g
                                    [ SvgA.transform
                                        [ Svg.Translate 0 (38 * (1 + index |> toFloat)) ]
                                    ]
                        )
                    |> Svg.g
                        [ SvgA.transform
                            [ Svg.Translate 0 12 ]
                        ]
                ]
                    |> Svg.g []

            Conversation.Choice choice ->
                case choice of
                    [ speakerChoice ] ->
                        [ speakerChoice.speaker
                            |> speakerUi
                        , speakerChoice.possibilities
                            |> List.indexedMap
                                (\index possibility ->
                                    possibility.lines
                                        |> possibilityUi
                                            ([ Svg.onClick
                                                ({ speaker = speakerChoice.speaker
                                                 , lines = possibility.lines
                                                 , state = possibility.state
                                                 }
                                                    |> ConversationPossibilityClicked
                                                )
                                             , SvgA.cursor Svg.CursorPointer
                                             ]
                                                ++ (if index == conversation.possibilityWatched then
                                                        [ SvgA.fontWeight Svg.FontWeightBolder
                                                        ]

                                                    else
                                                        [ Svg.onMouseEnter (ConversationChoicePossibilityWatched index)
                                                        ]
                                                   )
                                            )
                                        |> List.singleton
                                        |> Svg.g
                                            [ SvgA.transform
                                                [ Svg.Translate 0 (38 * (1 + index |> toFloat))
                                                ]
                                            ]
                                )
                            |> Svg.g
                                [ SvgA.transform
                                    [ Svg.Translate 0 12 ]
                                ]
                        ]
                            |> Svg.g []

                    [] ->
                        [ """__the chat left the chat__
You've reached a dead end where no choices or predetermined conversions are open
(If you believe this is a bug, please report under <https://github.com/lue-bird/where/issues/>)
"""
                            |> SvgCore.text
                        ]
                            |> Svg.text_
                                [ SvgA.fill (Svg.Paint (rgb 1 1 1))
                                ]

                    _ :: _ :: _ ->
                        [ """__the chat joined the chat__
Multiple choices are open
This is a bug, please report under <https://github.com/lue-bird/where/issues/>
"""
                            |> SvgCore.text
                        ]
                            |> Svg.text_ [ SvgA.fill (Svg.Paint (rgb 1 1 1)) ]


conversationHistoryUi :
    List
        { speaker : Where.Conversation.Speaker
        , lines : Where.Conversation.Lines
        }
    -> Svg event_
conversationHistoryUi =
    \conversationHistory ->
        conversationHistory
            |> List.foldl
                (\speakerChoice { index, list, height } ->
                    let
                        scale =
                            (1 / ((index |> toFloat) + 1)) ^ 0.35
                    in
                    { index = index + 1
                    , height = height - scale * 85
                    , list =
                        list
                            |> (::)
                                ([ speakerChoice.speaker |> speakerUi
                                 , speakerChoice.lines
                                    |> possibilityUi []
                                    |> List.singleton
                                    |> Svg.g
                                        [ SvgA.transform
                                            [ Svg.Translate 0 40 ]
                                        ]
                                 ]
                                    |> Svg.g
                                        [ SvgA.transform
                                            [ Svg.Scale scale scale
                                            ]
                                        ]
                                    |> List.singleton
                                    |> Svg.g
                                        [ SvgA.transform
                                            [ Svg.Translate 0 height
                                            ]
                                        ]
                                )
                    }
                )
                { index = 0, list = [], height = 0 }
            |> .list
            |> Svg.g
                []


possibilityUi :
    List (SvgCore.Attribute event)
    -> Where.Conversation.Lines
    -> Svg event
possibilityUi attributes =
    \conversationPossibility ->
        conversationPossibility
            |> List.map
                (\tellable ->
                    case tellable of
                        Where.Conversation.Item item ->
                            [ SvgCore.text "{ }"
                            , item |> itemUi
                            ]
                                |> Svg.tspan []

                        Where.Conversation.Text text ->
                            text |> SvgCore.text
                )
            |> (::) ("| " |> SvgCore.text)
            |> Svg.text_
                ([ SvgA.fontFamily [ "monospace" ]
                 , SvgA.fontSize (Svg.px 28)
                 , SvgA.fontWeight Svg.FontWeightNormal
                 , SvgA.fill (Svg.Paint (rgb 1 1 1))
                 ]
                    ++ attributes
                )


itemUi : Where.Conversation.Item -> Svg event_
itemUi =
    \item ->
        case item of
            Where.Conversation.TODO ->
                itemTODOUi


itemTODOUi =
    [ Svg.rect
        [ SvgA.width (Svg.px 1.03)
        , SvgA.height (Svg.px 1.02)
        , SvgA.fill (Svg.Paint (rgb 1 1 1))
        ]
        []
    , [ Svg.circle
            [ SvgA.r (Svg.px 0.14)
            , SvgA.cx (Svg.px -0.2)
            , SvgA.cy (Svg.px 0.25)
            , SvgA.fill (Svg.Paint (rgb 0 0 0))
            ]
            []
      , Svg.circle
            [ SvgA.r (Svg.px 0.14)
            , SvgA.cx (Svg.px 0.2)
            , SvgA.cy (Svg.px 0.25)
            , SvgA.fill (Svg.Paint (rgb 0 0 0))
            ]
            []
      , Svg.circle
            [ SvgA.r (Svg.px 0.14)
            , SvgA.cx (Svg.px 0)
            , SvgA.cy (Svg.px -0.1)
            , SvgA.fill (Svg.Paint (rgb 0 0 0))
            ]
            []
      ]
        |> Svg.g [ SvgA.transform [ Svg.Translate 0.5 0.425 ] ]
    ]
        |> Svg.g []


speakerUi : Where.Conversation.Speaker -> Svg event_
speakerUi =
    \speaker ->
        [ speaker
            |> Where.Conversation.speakerToString
            |> SvgCore.text
        ]
            |> Svg.text_
                [ SvgA.fontFamily [ "monospace" ]
                , SvgA.fontSize (Svg.px 37)
                , SvgA.fontWeight Svg.FontWeightBold
                , SvgA.fill (Svg.Paint (rgb 1 0.8 1))
                ]



-- audio


audio : AudioData -> State -> Audio
audio _ =
    \_ ->
        Audio.silence



-- util


arcTo : Svg.PathD.Point -> Svg.PathD.Segment
arcTo end =
    Svg.PathD.A ( 1, 1 ) 0 False False end
