module Game exposing (main)

{-| Flue

  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-geometry/latest>
  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/>
  - <https://dark.elm.dmy.fr/packages/elm-community/typed-svg/latest/>
  - <https://dark.elm.dmy.fr/packages/Spaxe/svg-pathd/latest/Svg-PathD>
  - <https://dark.elm.dmy.fr/packages/Herteby/simplex-noise/latest/>
  - <https://www.youtube.com/watch?v=CoMHkeAXEUY>
  - <https://dark.elm.dmy.fr/packages/xilnocas/step/latest/Step>

-}

import Angle
import Axis2d
import Browser
import Browser.Dom
import Browser.Events as Browser
import Color exposing (Color, rgb, rgb255, rgba)
import Direction2d
import Duration exposing (Duration, Seconds)
import Html exposing (Html)
import Html.Attributes as Html
import Keyboard exposing (Key, KeyChange(..))
import Keyboard.Arrows
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Pixels exposing (Pixels, PixelsPerSecond)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Progress exposing (Progress(..))
import Quantity exposing (Quantity, Rate, Unitless, in_)
import Random
import Random.Extra as Random
import Random.List
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rectangle2d exposing (Rectangle2d)
import Simplex
import Speed exposing (MetersPerSecond, Speed)
import Step exposing (Step)
import Svg.PathD
import Task
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Filters as SvgFilter
import TypedSvg.Filters.Attributes as SvgFilterA
import TypedSvg.Types as Svg exposing (Paint)
import Vector2d exposing (Vector2d)


main : Program () Model Event
main =
    Browser.element
        { init = init
        , subscriptions = subscribe
        , update = reactTo |> Step.asUpdateFunction
        , view = htmlUi
        }


type alias Model =
    RecordWithoutConstructorFunction
        { seed : Random.Seed
        , permutations : Simplex.PermutationTable
        , birdPosition : Point2d Pixels Float
        , birdSpeed : Vector2d PixelsPerSecond Float
        , birdMove : BirdMove
        , sceneChunkLeft : SceneChunk
        , sceneChunkRight : SceneChunk
        , windowWidth : Quantity Float Pixels
        , windowHeight : Quantity Float Pixels
        , keysPressed : List Key
        , keyUpChanges : List Key
        , keyDownChanges : List Key
        }


type BirdMove
    = Sitting
    | Flying Flying


type Flying
    = FlapWarmingUp Progress
    | Flapping FlapDirection { power : Float } Progress


type alias SceneInteractionPart =
    RecordWithoutConstructorFunction
        { kind : SceneEntityKind
        , position : Point2d Pixels Float
        }


type SceneEntityKind
    = Tree TreeSpecific
    | Shrub ShrubSpecific
    | Grass GrassSpecific


type alias TreeSpecific =
    ()


type alias TreeSmallSpecific =
    ()


type alias ShrubSpecific =
    ()


type alias GrassSpecific =
    ()


type Event
    = WindowSized
        { width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    | KeyEvent Keyboard.Msg
    | AnimationFramePassed { delta : Duration }
    | PermutationsGenerated Simplex.PermutationTable


sceneChunkEmpty : SceneChunk
sceneChunkEmpty =
    { interactive =
        { ground = []
        , entities =
            [ { kind = Tree ()
              , position = Point2d.pixels 0 0
              }
            ]
        }
    , background =
        { ground = []
        , entities = []
        }
    }


init : () -> ( Model, Cmd Event )
init () =
    ( { seed =
            -- dummy. Will be replaced
            Random.initialSeed 1649952663
      , permutations =
            -- dummy. Will be replaced
            Simplex.permutationTableFromInt 1649952663
      , birdPosition = Point2d.pixels 0 400
      , birdSpeed =
            Vector2d.pixels 0 0
                |> Vector2d.per Duration.second
      , sceneChunkLeft = sceneChunkEmpty
      , sceneChunkRight = sceneChunkEmpty
      , windowWidth = Pixels.pixels 1920
      , windowHeight = Pixels.pixels 1080
      , keysPressed = []
      , keyUpChanges = []
      , keyDownChanges = []
      , birdMove = Sitting
      }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform
                (\{ viewport } ->
                    { width = Pixels.pixels viewport.width
                    , height = Pixels.pixels viewport.height
                    }
                        |> WindowSized
                )
        , Simplex.permutationTableGenerator
            |> Random.generate PermutationsGenerated
        ]
    )


subscribe : Model -> Sub Event
subscribe =
    \model ->
        Sub.batch
            [ Browser.onAnimationFrameDelta
                (\deltaMilliseconds ->
                    AnimationFramePassed
                        { delta = Duration.milliseconds deltaMilliseconds }
                )
            , Browser.onResize
                (\width height ->
                    { width = Pixels.pixels (width |> toFloat)
                    , height = Pixels.pixels (height |> toFloat)
                    }
                        |> WindowSized
                )
            , Keyboard.subscriptions |> Sub.map KeyEvent
            ]


type FlapDirection
    = FlapLeft
    | FlapRight


keysToFlapDirection : List Key -> Maybe FlapDirection
keysToFlapDirection =
    \keys ->
        case (keys |> Keyboard.Arrows.arrows).x + 1 of
            0 ->
                FlapLeft |> Just

            2 ->
                FlapRight |> Just

            _ ->
                Nothing


reactTo : Event -> Model -> Step Model Event exit_
reactTo event =
    case event of
        WindowSized size ->
            \model ->
                Step.to
                    { model
                        | windowWidth = size.width
                        , windowHeight = size.height
                    }

        KeyEvent keyEvent ->
            \model ->
                let
                    ( keysPressedUpdated, maybeKeyChange ) =
                        model.keysPressed
                            |> Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyEvent

                    keyChangesUpdated =
                        case maybeKeyChange of
                            Just (KeyUp keyChange) ->
                                { model
                                    | keyUpChanges =
                                        model.keyUpChanges |> (::) keyChange
                                }

                            Just (KeyDown keyChange) ->
                                { model
                                    | keyDownChanges =
                                        model.keyDownChanges |> (::) keyChange
                                }

                            Nothing ->
                                model
                in
                Step.to
                    { keyChangesUpdated
                        | keysPressed = keysPressedUpdated
                    }

        PermutationsGenerated permutations ->
            \model ->
                let
                    chunkRandom { startX } =
                        sceneChunkRandom
                            { startX = startX
                            , chunkWidth = model.windowWidth
                            , permutations = permutations
                            }
                in
                Step.to
                    { model
                        | permutations = permutations
                        , sceneChunkLeft = chunkRandom { startX = model.windowWidth |> Quantity.negate }
                        , sceneChunkRight = chunkRandom { startX = Pixels.float 0 }
                    }

        AnimationFramePassed { delta } ->
            \current ->
                let
                    speedNo : Vector2d PixelsPerSecond Float
                    speedNo =
                        Vector2d.pixels 0 0
                            |> Vector2d.per Duration.second

                    flapSpeed direction progress { power } =
                        let
                            fraction =
                                progress |> Progress.toFraction
                        in
                        Vector2d.fromRecord Pixels.pixels
                            { x =
                                (case direction of
                                    FlapLeft ->
                                        -1

                                    FlapRight ->
                                        1
                                )
                                    * power
                                    * 15
                                    * (1 - fraction)
                            , y = power * 18 * (1 - fraction)
                            }
                            |> Vector2d.per Duration.second

                    updatedMove : { move : BirdMove, flapSpeed : Vector2d PixelsPerSecond Float }
                    updatedMove =
                        case current.birdMove of
                            Sitting ->
                                { move =
                                    case current.keyDownChanges |> keysToFlapDirection of
                                        Just flapDirection ->
                                            FlapWarmingUp Progress.begin
                                                |> Flying

                                        Nothing ->
                                            Sitting
                                , flapSpeed = speedNo
                                }

                            Flying (FlapWarmingUp flapWarmUpProgress) ->
                                { move =
                                    case current.keyUpChanges |> keysToFlapDirection of
                                        Just flapDirection ->
                                            let
                                                fraction =
                                                    flapWarmUpProgress |> Progress.toFraction
                                            in
                                            Flapping flapDirection
                                                { power = fraction }
                                                Progress.begin
                                                |> Flying

                                        Nothing ->
                                            FlapWarmingUp
                                                (flapWarmUpProgress
                                                    |> Progress.by
                                                        { delta = delta
                                                        , ready = warmupReadyDuration
                                                        }
                                                )
                                                |> Flying
                                , flapSpeed = speedNo
                                }

                            Flying (Flapping currentFlapDirection power flappingProgress) ->
                                { move =
                                    case current.keyDownChanges |> keysToFlapDirection of
                                        Just flapDirection ->
                                            FlapWarmingUp Progress.begin
                                                |> Flying

                                        Nothing ->
                                            Flapping
                                                currentFlapDirection
                                                power
                                                (flappingProgress
                                                    |> Progress.by
                                                        { delta = delta
                                                        , ready = flapDuration
                                                        }
                                                )
                                                |> Flying
                                , flapSpeed =
                                    flapSpeed currentFlapDirection flappingProgress power
                                }

                    collision : Maybe (Point2d Pixels Float)
                    collision =
                        let
                            groundSegments =
                                current.sceneChunkLeft.interactive.ground
                                    ++ current.sceneChunkRight.interactive.ground
                                    |> List.map .position
                                    |> Polyline2d.fromVertices
                                    |> Polyline2d.segments

                            birdSegment =
                                LineSegment2d.fromPointAndVector
                                    current.birdPosition
                                    (Vector2d.pixels (birdWidth * 0.8) 0)
                        in
                        groundSegments
                            |> List.filterMap
                                (\segment ->
                                    segment |> LineSegment2d.intersectionPoint birdSegment
                                )
                            |> List.head

                    updatedBirdSpeed =
                        (case collision of
                            Nothing ->
                                current.birdSpeed
                                    |> Vector2d.plus
                                        (gravity |> Vector2d.for delta)

                            Just collisionPoint ->
                                speedNo
                        )
                            |> vector2dMapXY
                                { x =
                                    Quantity.multiplyBy
                                        (1
                                            - (Quantity.float 0.45
                                                |> Quantity.per Duration.second
                                                |> Quantity.for delta
                                                |> Quantity.toFloat
                                              )
                                        )
                                , y =
                                    Quantity.multiplyBy
                                        (1
                                            - (Quantity.float 0.3
                                                |> Quantity.per Duration.second
                                                |> Quantity.for delta
                                                |> Quantity.toFloat
                                              )
                                        )
                                }
                            |> Vector2d.plus updatedMove.flapSpeed
                in
                Step.to
                    { current
                        | birdMove =
                            case current.birdMove of
                                Sitting ->
                                    updatedMove.move

                                Flying _ ->
                                    case collision of
                                        Nothing ->
                                            updatedMove.move

                                        Just _ ->
                                            updatedMove.move

                        -- todo Sitting
                        , birdPosition =
                            current.birdPosition
                                |> Point2d.translateBy
                                    (updatedBirdSpeed
                                        |> Vector2d.for delta
                                    )
                        , birdSpeed = updatedBirdSpeed
                        , keyUpChanges =
                            current.keyUpChanges
                                |> List.filterMap
                                    (\key ->
                                        case [ key ] |> keysToFlapDirection of
                                            Nothing ->
                                                key |> Just

                                            Just _ ->
                                                Nothing
                                    )
                        , keyDownChanges =
                            current.keyDownChanges
                                |> List.filterMap
                                    (\key ->
                                        case [ key ] |> keysToFlapDirection of
                                            Nothing ->
                                                key |> Just

                                            Just _ ->
                                                Nothing
                                    )
                    }


type alias SceneChunk =
    RecordWithoutConstructorFunction
        { background : SceneChunkLayer
        , interactive : SceneChunkLayer
        }


type alias SceneChunkLayer =
    RecordWithoutConstructorFunction
        { ground : Ground
        , entities : List SceneEntity
        }


type alias SceneEntity =
    RecordWithoutConstructorFunction
        { kind : SceneEntityKind
        , position : Point2d Pixels Float
        }


type alias Ground =
    List { position : Point2d Pixels Float }


sceneChunkRandom :
    { startX : Quantity Float Pixels
    , chunkWidth : Quantity Float Pixels
    , permutations : Simplex.PermutationTable
    }
    -> SceneChunk
sceneChunkRandom { startX, chunkWidth, permutations } =
    let
        sceneChunkLayer { scaling, layer } =
            sceneChunkLayerRandom
                { startX = startX
                , chunkWidth = chunkWidth
                , permutations = permutations
                , scaling = scaling
                , layer = layer
                }
    in
    { background = sceneChunkLayer { scaling = 0.9, layer = -1 }
    , interactive = sceneChunkLayer { scaling = 1, layer = 0 }
    }


sceneChunkLayerRandom :
    { startX : Quantity Float Pixels
    , chunkWidth : Quantity Float Pixels
    , permutations : Simplex.PermutationTable
    , scaling : Float
    , layer : Float
    }
    -> SceneChunkLayer
sceneChunkLayerRandom { startX, chunkWidth, permutations, scaling, layer } =
    let
        ground =
            groundRandom
                { startX = startX
                , scaling = scaling
                , chunkWidth = chunkWidth
                , permutations = permutations
                , layer = layer
                }
    in
    { ground = ground
    , entities =
        sceneEntitiesRandom
            { ground = ground
            , permutations = permutations
            }
    }


noise :
    { permutations : Simplex.PermutationTable
    , x : Quantity Float Pixels
    , layer : Float
    , scaling : Float
    }
    -> Quantity Float Pixels
noise { permutations, x, scaling, layer } =
    let
        negToPos1 =
            Simplex.fractal2d
                { scale = 4.3 * scaling
                , steps = (6 * scaling) |> round
                , stepSize = 4.2 * scaling
                , persistence = 2.2 * scaling
                }
                permutations
                (x |> Pixels.toFloat)
                layer
    in
    ((negToPos1 + 1) / 2) |> Pixels.float


groundRandom :
    { permutations : Simplex.PermutationTable
    , scaling : Float
    , startX : Quantity Float Pixels
    , chunkWidth : Quantity Float Pixels
    , layer : Float
    }
    -> Ground
groundRandom { startX, chunkWidth, permutations, scaling, layer } =
    let
        endX =
            chunkWidth |> Quantity.minus startX
    in
    List.iterate
        (\x ->
            if x |> Quantity.lessThan endX then
                (x |> Quantity.plus (Pixels.pixels 22)) |> Just

            else
                Nothing
        )
        startX
        |> List.map
            (\x ->
                { position =
                    Point2d.xy
                        x
                        (noise
                            { permutations = permutations
                            , scaling = scaling
                            , x = x
                            , layer = layer
                            }
                            |> Quantity.multiplyBy 600
                        )
                }
            )


sceneEntitiesRandom :
    { ground : Ground
    , permutations : Simplex.PermutationTable
    }
    -> List SceneInteractionPart
sceneEntitiesRandom { ground, permutations } =
    let
        entityNoise { scaling, groundPoint } =
            noise
                { permutations = permutations
                , x = groundPoint.position |> Point2d.xCoordinate
                , layer = 0
                , scaling = scaling
                }

        groundIndexed =
            ground
                |> List.indexedMap
                    (\index groundPoint ->
                        { index = index, groundPoint = groundPoint }
                    )
    in
    [ groundIndexed
        |> List.filter (\{ index } -> (index |> modBy 2) == 0)
        |> List.filterMap
            (\{ groundPoint } ->
                if
                    entityNoise { groundPoint = groundPoint, scaling = 0.7 }
                        |> Quantity.greaterThan (Pixels.float 0.5)
                then
                    { kind = Grass ()
                    , position =
                        groundPoint.position
                            |> Point2d.translateBy (Vector2d.pixels 0 15)
                    }
                        |> Just

                else
                    Nothing
            )
    , groundIndexed
        |> List.filter (\{ index } -> (index |> modBy 10) == 0)
        |> List.filterMap
            (\{ groundPoint } ->
                if
                    entityNoise { groundPoint = groundPoint, scaling = 0.85 }
                        |> Quantity.greaterThan (Pixels.float 0.5)
                then
                    { kind = Shrub ()
                    , position =
                        groundPoint.position
                            |> Point2d.translateBy (Vector2d.pixels 0 -25)
                    }
                        |> Just

                else
                    Nothing
            )
    , groundIndexed
        |> List.filter (\{ index } -> (index |> modBy 17) == 0)
        |> List.filterMap
            (\{ groundPoint } ->
                if
                    entityNoise { groundPoint = groundPoint, scaling = 0.9 }
                        |> Quantity.greaterThan (Pixels.float 0.5)
                then
                    { kind = Tree ()
                    , position =
                        groundPoint.position
                            |> Point2d.translateBy (Vector2d.pixels 0 -30)
                    }
                        |> Just

                else
                    Nothing
            )
    ]
        |> List.concat


warmupReadyDuration : Duration
warmupReadyDuration =
    Duration.seconds 0.18


flapDuration : Duration
flapDuration =
    Duration.seconds 0.5


gravity : Vector2d (Rate (Rate Pixels Seconds) Seconds) coordinates
gravity =
    Vector2d.pixels 0 -140
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


htmlUi : Model -> Html Event
htmlUi =
    \model ->
        model
            |> ui
            |> List.singleton
            |> svg
                [ SvgA.viewBox
                    0
                    0
                    (model.windowWidth |> Pixels.toFloat)
                    ((model.windowHeight |> Pixels.toFloat) - 10)
                , Html.style "width" "100%"
                ]


ui : Model -> Svg event_
ui =
    \{ birdPosition, birdSpeed, windowWidth, windowHeight, birdMove, sceneChunkLeft, sceneChunkRight } ->
        [ Svg.defs
            []
            [ Svg.radialGradient
                [ SvgA.id "background"
                , SvgA.r (Svg.percent 150)
                ]
                [ Svg.stop
                    [ SvgA.offset "37%"
                    , SvgA.stopColor (rgb 0.4 0.5 0.7 |> Color.toCssString)
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
        , Svg.rect
            [ SvgA.fill (Svg.Reference "background")
            , SvgA.width (Svg.percent 100)
            , SvgA.height (Svg.percent 100)
            ]
            []
        , [ [ sceneChunkLeft, sceneChunkRight ]
                |> List.map
                    (sceneUi
                        { birdPosition = birdPosition
                        , birdSpeed = birdSpeed
                        , chunkWidth = windowWidth
                        }
                    )
                |> Svg.g []
          , birdUi { move = birdMove }
          ]
            |> Svg.g
                [ SvgA.transform
                    [ Svg.Scale 1 -1
                    , translateTo
                        (Point2d.pixels
                            (windowWidth |> Quantity.multiplyBy 0.5 |> Pixels.toFloat)
                            (windowHeight
                                |> Quantity.negate
                                |> Quantity.plus
                                    (let
                                        screenBirdY =
                                            windowHeight |> Quantity.multiplyBy 0.4

                                        actualBirdY =
                                            birdPosition |> Point2d.yCoordinate
                                     in
                                     if screenBirdY |> Quantity.lessThan actualBirdY then
                                        screenBirdY

                                     else
                                        actualBirdY
                                    )
                                |> Pixels.toFloat
                            )
                        )
                    ]
                ]
        , [ Svg.rect
                [ SvgA.fill (Svg.Paint Color.black)
                , SvgA.width (Svg.percent 100)
                , SvgA.height (Svg.percent 8)
                ]
                []
          , Svg.rect
                [ SvgA.fill (Svg.Paint Color.black)
                , SvgA.width (Svg.percent 100)
                , SvgA.height (Svg.percent 8)
                , SvgA.y (Svg.percent 92)
                ]
                []
          ]
            |> Svg.g []
        ]
            |> Svg.g []


birdHeight =
    5


birdWidth =
    51


birdUi :
    { move : BirdMove }
    -> Svg event_
birdUi { move } =
    [ let
        directionY =
            case move of
                Sitting ->
                    0

                Flying flap ->
                    case flap of
                        FlapWarmingUp progress ->
                            -4 + birdHeight - ((progress |> Progress.toFraction) * birdHeight)

                        Flapping _ { power } progress ->
                            -4
                                + (case progress of
                                    InProgress fraction ->
                                        fraction * birdHeight

                                    Ready ->
                                        birdHeight
                                  )
      in
      Svg.path
        [ SvgA.d
            (Svg.PathD.pathD
                [ Svg.PathD.M
                    ( 0, birdHeight / 2 )
                , Svg.PathD.Q
                    ( -birdWidth / 4, directionY * 0.8 + 10 )
                    ( -birdWidth / 2, -directionY )
                , Svg.PathD.Q
                    ( -birdWidth / 4, directionY * 0.8 )
                    ( 0, -birdHeight / 2 )
                , Svg.PathD.Q
                    ( birdWidth / 4, directionY * 0.8 )
                    ( birdWidth / 2, -directionY )
                , Svg.PathD.Q
                    ( birdWidth / 4, directionY * 0.8 + 10 )
                    ( 0, birdHeight / 2 )
                , Svg.PathD.Z
                ]
            )
        , SvgA.fill (Svg.Paint (rgb 0.8 0.9 1))
        , SvgA.filter (Svg.Filter "blur")
        ]
        []
    ]
        |> Svg.g []


sceneUi :
    { birdPosition : Point2d Pixels Float
    , birdSpeed : Vector2d PixelsPerSecond Float
    , chunkWidth : Quantity Float Pixels
    }
    -> SceneChunk
    -> Svg event_
sceneUi { birdPosition, chunkWidth } =
    \chunk ->
        [ [ chunk.background
                |> sceneLayerUi
                    { chunkWidth = chunkWidth }
                    [ SvgA.opacity (Svg.Opacity 0.4)
                    ]
          , chunk.interactive
                |> sceneLayerUi
                    { chunkWidth = chunkWidth }
                    []
          ]
            |> Svg.g
                [ SvgA.transform
                    [ translateTo
                        (birdPosition
                            |> Point2d.rotateAround
                                Point2d.origin
                                (Angle.turns 0.5)
                        )
                    ]
                ]
        ]
            |> Svg.g []


sceneLayerUi :
    { chunkWidth : Quantity Float Pixels }
    -> List (Svg.Attribute event)
    -> SceneChunkLayer
    -> Svg event
sceneLayerUi chunkWidth attributes =
    \sceneLayer ->
        [ sceneLayer.entities
            |> List.map sceneEntityUi
            |> Svg.g []
        , sceneLayer.ground |> groundUi chunkWidth
        ]
            |> Svg.g attributes


sceneEntityUi : SceneEntity -> Svg event_
sceneEntityUi =
    \{ kind, position } ->
        [ case kind of
            Tree treeSpecific ->
                treeUi treeSpecific

            Shrub shrubSpecific ->
                shrubUi shrubSpecific

            Grass grassSpecific ->
                grassUi grassSpecific
        ]
            |> Svg.g
                [ SvgA.transform [ translateTo position ]
                ]


groundUi :
    { chunkWidth : Quantity Float Pixels }
    -> Ground
    -> Svg event_
groundUi { chunkWidth } =
    \ground ->
        let
            startX =
                ground
                    |> List.head
                    |> Maybe.map .position
                    |> Maybe.map Point2d.xCoordinate
                    |> Maybe.withDefault Quantity.zero
        in
        Svg.path
            [ SvgA.fill (Svg.Paint (rgb 0.07 0.04 0))
            , SvgA.d
                (Svg.PathD.pathD
                    (Svg.PathD.M ( startX |> Pixels.toFloat, 0 )
                        :: (ground
                                |> List.map
                                    (\{ position } ->
                                        Svg.PathD.L (position |> Point2d.toTuple (in_ Pixels.pixels))
                                    )
                           )
                        ++ [ Svg.PathD.L
                                ( startX |> Quantity.plus chunkWidth |> Pixels.toFloat, 0 )
                           , Svg.PathD.Z
                           ]
                    )
                )
            ]
            []


grassUi : GrassSpecific -> Svg event_
grassUi () =
    List.range 1 3
        |> List.map
            (\i ->
                let
                    width =
                        (i |> toFloat) * 12
                in
                Svg.path
                    [ SvgA.stroke (Svg.Paint (rgb 0 0.4 0.25))
                    , SvgA.strokeWidth (Svg.px 3)
                    , SvgA.fill (Svg.Paint transparent)
                    , SvgA.d
                        (Svg.PathD.pathD
                            [ Svg.PathD.M ( width / 2, 0 )
                            , arcTo ( -width / 2, 0 )
                            ]
                        )
                    ]
                    []
            )
        |> Svg.g []


treeUi : TreeSpecific -> Svg event_
treeUi () =
    let
        leafPart { color, width } =
            Svg.path
                [ SvgA.fill (Svg.Paint transparent)
                , SvgA.stroke (Svg.Paint color)
                , SvgA.strokeWidth (Svg.px 8)
                , SvgA.d
                    (Svg.PathD.pathD
                        [ Svg.PathD.M ( width / 2, 0 )
                        , arcTo ( -width / 2, 0 )
                        ]
                    )
                ]
                []

        height =
            200

        leafPartCount =
            3
    in
    [ Svg.path
        [ SvgA.fill (Svg.Paint transparent)
        , SvgA.stroke (Svg.Paint (rgb 0 0.19 0.19))
        , SvgA.strokeWidth (Svg.px 13)
        , SvgA.d
            (Svg.PathD.pathD
                [ Svg.PathD.M ( 0, 0 )
                , Svg.PathD.L ( 0, height )
                ]
            )
        ]
        []
    , List.range 0 (leafPartCount - 1)
        |> List.map
            (\index ->
                let
                    width =
                        (2 + (index |> toFloat)) * 35
                in
                [ leafPart
                    { width = width
                    , color =
                        rgb255 (35 + index * 10) (66 + index * 26) (54 + index * 22)
                    }
                ]
                    |> Svg.g
                        [ SvgA.transform
                            [ translateTo
                                (Point2d.pixels
                                    0
                                    (((leafPartCount - index) |> toFloat)
                                        * (height / leafPartCount)
                                    )
                                )
                            ]
                        ]
            )
        |> List.reverse
        |> Svg.g []
    ]
        |> Svg.g []


shrubUi : ShrubSpecific -> Svg event_
shrubUi () =
    let
        leafPart { color, width } =
            Svg.path
                [ SvgA.fill (Svg.Paint color)
                , SvgA.d
                    (Svg.PathD.pathD
                        [ Svg.PathD.M ( 0, 0 )
                        , Svg.PathD.L ( -width / 2, 0 )
                        , Svg.PathD.L ( 0, 20 + width * 0.8 )
                        , Svg.PathD.L ( width / 2, 0 )
                        , Svg.PathD.L ( 0, 0 )
                        ]
                    )
                ]
                []

        leafPartCount =
            3
    in
    List.range 0 (leafPartCount - 1)
        |> List.map
            (\index ->
                let
                    indexFloat =
                        index |> toFloat
                in
                [ leafPart
                    { width = (4 + indexFloat) * 20
                    , color =
                        rgb (0.099 - indexFloat * 0.03)
                            (0.128 - indexFloat * 0.022)
                            (0.054 - indexFloat * 0.19)
                    }
                ]
                    |> Svg.g
                        [ SvgA.transform
                            [ translateTo
                                (Point2d.pixels
                                    0
                                    ((index |> toFloat) * 24)
                                )
                            ]
                        ]
            )
        |> Svg.g []



--


vector2dMapXY :
    { x : Quantity Float units -> Quantity Float mappedUnits
    , y : Quantity Float units -> Quantity Float mappedUnits
    }
    -> Vector2d units coordinates
    -> Vector2d mappedUnits mappedCoordinates_
vector2dMapXY changes =
    \vector ->
        Vector2d.xy
            ((vector |> Vector2d.xComponent)
                |> changes.x
            )
            ((vector |> Vector2d.yComponent)
                |> changes.y
            )


translateTo : Point2d Pixels Float -> Svg.Transform
translateTo position =
    let
        point =
            position |> Point2d.toPixels
    in
    Svg.Translate point.x point.y


arcTo : Svg.PathD.Point -> Svg.PathD.Segment
arcTo end =
    Svg.PathD.A ( 1, 1 ) 0 False False end


transparent : Color
transparent =
    rgba 0 0 0 0
