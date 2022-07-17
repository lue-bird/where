module Game exposing (main)

{-| GMTK Game Jam 2022

  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-geometry/latest>
  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/>
  - <https://dark.elm.dmy.fr/packages/elm-community/typed-svg/latest/>
  - <https://dark.elm.dmy.fr/packages/Spaxe/svg-pathd/latest/Svg-PathD>
  - <https://dark.elm.dmy.fr/packages/Herteby/simplex-noise/latest/>
  - <https://www.youtube.com/watch?v=CoMHkeAXEUY>
  - <https://dark.elm.dmy.fr/packages/xilnocas/step/latest/Step>

-}

import Angle
import Array
import ArraySized exposing (ArraySized)
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
import Linear exposing (DirectionLinear(..))
import List.Extra as List
import N exposing (Exactly, In, N, N0, n0, n1)
import Ns exposing (N31, N32, N63, N64, n31, n32, n62, n63, n64)
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
        , playerPosition : Point2d Pixels Float
        , playerSpeed : Vector2d (Rate Pixels Seconds) Float
        , playerWarmUp : Progress
        , scene : Scene
        , windowWidth : Quantity Float Pixels
        , windowHeight : Quantity Float Pixels
        , keysPressed : List Key
        , keyUpChanges : List Key
        , keyDownChanges : List Key

        -- changed by dice roll
        {-
           - upside down
           - swapped controls
           - slippery controls
           - speed boost
           - switched dice positions
           - new terrain
           - more obstacles (traps, spikes, ...)
           - whether, wind
           - available tools (bombs, shovel)
        -}
        , flip : Bool
        }


type alias SceneInteractionPart =
    RecordWithoutConstructorFunction
        { kind : Tile
        , position : Point2d Pixels Float
        }


type Tile
    = Air AirSpecific
    | Collidable TileCollidable


tileIsCollidable : Tile -> Maybe TileCollidable
tileIsCollidable =
    \tile ->
        case tile of
            Air _ ->
                Nothing

            Collidable collidable ->
                collidable |> Just


type TileCollidable
    = Ground GroundSpecific
    | Grass GrassSpecific
    | Trunk TrunkSpecific
    | Leaf TreeSpecific
    | Shrub ShrubSpecific
    | Dice DiceSpecific


type alias AirSpecific =
    ()


type alias GroundSpecific =
    ()


type alias TreeSpecific =
    ()


type alias ShrubSpecific =
    ()


type alias GrassSpecific =
    ()


type alias TrunkSpecific =
    ()


type alias DiceSpecific =
    ()


type Event
    = WindowSized
        { width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    | KeyEvent Keyboard.Msg
    | AnimationFramePassed { delta : Duration }
    | PermutationsGenerated Simplex.PermutationTable


sceneAir : Scene
sceneAir =
    ArraySized.repeat n64
        (ArraySized.repeat n32
            (Air ())
        )


init : () -> ( Model, Cmd Event )
init () =
    ( { seed =
            -- dummy. Will be replaced
            Random.initialSeed 1329952631
      , permutations =
            -- dummy. Will be replaced
            Simplex.permutationTableFromInt 1329952631
      , playerPosition = Point2d.pixels 0 0
      , playerSpeed =
            Vector2d.pixels 0 0
                |> Vector2d.per Duration.second
      , playerWarmUp = Progress.begin
      , scene = sceneAir
      , windowWidth = Pixels.pixels 1920
      , windowHeight = Pixels.pixels 1080
      , keysPressed = []
      , keyUpChanges = []
      , keyDownChanges = []
      , flip = False
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


type XDirection
    = Left
    | Right


keysXDirection : List Key -> Maybe XDirection
keysXDirection =
    \keys ->
        case (keys |> Keyboard.Arrows.arrows).x + 1 of
            0 ->
                Left |> Just

            2 ->
                Right |> Just

            _ ->
                Nothing


keysYDirection : List Key -> Maybe DirectionLinear
keysYDirection =
    \keys ->
        case (keys |> Keyboard.Arrows.arrows).y + 1 of
            0 ->
                Down |> Just

            2 ->
                Up |> Just

            _ ->
                Nothing


playerPositionIndexInScene :
    Point2d Pixels Float
    ->
        { x :
            Result
                (N.BelowOrAbove Int (N (N.Min N64)))
                (N (In N0 N63 {}))
        , y :
            Result
                (N.BelowOrAbove Int (N (N.Min N32)))
                (N (In N0 N31 {}))
        }
playerPositionIndexInScene =
    \playerPosition ->
        { x =
            (playerPosition
                |> Point2d.xCoordinate
                |> Pixels.toFloat
                |> round
            )
                + 32
                |> N.intIsIn ( n0, n63 )
        , y =
            (playerPosition
                |> Point2d.yCoordinate
                |> Pixels.toFloat
                |> round
            )
                + 16
                |> N.intIsIn ( n0, n31 )
        }


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
                Step.to
                    { model
                        | permutations = permutations
                        , scene =
                            sceneRandom
                                { permutations = permutations }
                    }

        AnimationFramePassed { delta } ->
            \current ->
                let
                    playerFutureIndexInScene =
                        current.playerPosition
                            |> Point2d.translateBy
                                (current.playerSpeed |> Vector2d.for (Duration.seconds 0.055))
                            |> playerPositionIndexInScene

                    isBelowScene =
                        case playerFutureIndexInScene.y of
                            Ok _ ->
                                False

                            Err (N.Above _) ->
                                False

                            Err (N.Below _) ->
                                True

                    willCollideBelow : Maybe TileCollidable
                    willCollideBelow =
                        case ( playerFutureIndexInScene.x, playerFutureIndexInScene.y ) of
                            ( Ok x, Ok y ) ->
                                case y |> N.isAtLeast n1 of
                                    Err _ ->
                                        Nothing

                                    Ok indexInSceneYAtLeast1 ->
                                        current.scene
                                            |> ArraySized.element ( Up, x )
                                            |> ArraySized.element
                                                ( Up
                                                , indexInSceneYAtLeast1 |> N.minSub n1
                                                )
                                            |> tileIsCollidable

                            ( _, _ ) ->
                                Nothing

                    willCollideRight : Maybe TileCollidable
                    willCollideRight =
                        case ( playerFutureIndexInScene.x, playerFutureIndexInScene.y ) of
                            ( Ok x, Ok y ) ->
                                case x |> N.add n1 |> N.isAtMost n62 of
                                    Err _ ->
                                        Nothing

                                    Ok notQuiteRight ->
                                        current.scene
                                            |> ArraySized.element
                                                ( Up, notQuiteRight |> N.add n1 )
                                            |> ArraySized.element ( Up, y )
                                            |> tileIsCollidable

                            ( _, _ ) ->
                                Nothing

                    willCollideLeft : Maybe TileCollidable
                    willCollideLeft =
                        case ( playerFutureIndexInScene.x, playerFutureIndexInScene.y ) of
                            ( Ok x, Ok y ) ->
                                case x |> N.isAtLeast n1 of
                                    Err _ ->
                                        Nothing

                                    Ok notQuiteLeft ->
                                        current.scene
                                            |> ArraySized.element
                                                ( Up, notQuiteLeft |> N.minSub n1 )
                                            |> ArraySized.element ( Up, y )
                                            |> tileIsCollidable

                            ( _, _ ) ->
                                Nothing

                    updatedMoveY :
                        { warmUp : Progress
                        , speed :
                            Vector2d (Rate Pixels Seconds) Float
                            -> Vector2d (Rate Pixels Seconds) Float
                        }
                    updatedMoveY =
                        let
                            maybeWarmUp =
                                { warmUp =
                                    case current.keysPressed |> keysYDirection of
                                        Just Down ->
                                            current.playerWarmUp
                                                |> Progress.by
                                                    { delta = delta
                                                    , ready = warmUpDuration
                                                    }

                                        _ ->
                                            Progress.begin
                                , speed = identity
                                }

                            tryJump =
                                case willCollideBelow of
                                    Nothing ->
                                        { warmUp = current.playerWarmUp
                                        , speed = identity
                                        }

                                    Just _ ->
                                        { warmUp = Progress.begin
                                        , speed =
                                            Vector2d.plus
                                                (Vector2d.pixels
                                                    0
                                                    (7.5 * (current.playerWarmUp |> Progress.toFraction))
                                                    |> Vector2d.per Duration.second
                                                )
                                        }
                        in
                        case current.keysPressed |> keysYDirection of
                            Just Down ->
                                maybeWarmUp

                            Just Up ->
                                tryJump

                            Nothing ->
                                tryJump

                    playerSpeedUpdated : Vector2d (Rate Pixels Seconds) Float
                    playerSpeedUpdated =
                        current.playerSpeed
                            |> (case willCollideBelow of
                                    Nothing ->
                                        Vector2d.plus
                                            (gravity |> Vector2d.for delta)
                                            >> (case willCollideLeft of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        vector2dMapX (Quantity.abs >> Quantity.multiplyBy 0.6)
                                               )
                                            >> (case willCollideRight of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        vector2dMapX (Quantity.abs >> Quantity.multiplyBy -0.6)
                                               )

                                    Just _ ->
                                        vector2dMapY
                                            (Quantity.minus (Pixels.float 2 |> Quantity.per Duration.second)
                                                >> Quantity.multiplyBy -0.6
                                            )
                               )
                            |> vector2dMapX
                                (Quantity.multiplyBy
                                    (1
                                        - (Quantity.float 0.45
                                            |> Quantity.per Duration.second
                                            |> Quantity.for delta
                                            |> Quantity.toFloat
                                          )
                                    )
                                )
                            |> vector2dMapY
                                (Quantity.multiplyBy
                                    (1
                                        - (Quantity.float 0.3
                                            |> Quantity.per Duration.second
                                            |> Quantity.for delta
                                            |> Quantity.toFloat
                                          )
                                    )
                                )
                            |> (case current.keysPressed |> keysXDirection of
                                    Nothing ->
                                        identity

                                    Just direction ->
                                        vector2dMapX
                                            (Quantity.plus
                                                (Pixels.float
                                                    (case direction of
                                                        Left ->
                                                            -0.7

                                                        Right ->
                                                            0.7
                                                    )
                                                    |> Quantity.per Duration.second
                                                )
                                            )
                                            >> vector2dMapX
                                                (Quantity.clamp
                                                    (Pixels.float -4.9 |> Quantity.per Duration.second)
                                                    (Pixels.float 4.9 |> Quantity.per Duration.second)
                                                )
                               )
                            |> updatedMoveY.speed

                    onDice =
                        case ( playerFutureIndexInScene.x, playerFutureIndexInScene.y ) of
                            ( Ok x, Ok y ) ->
                                case y |> N.isAtLeast n1 of
                                    Err _ ->
                                        { scene = current.scene
                                        , flip = current.flip
                                        }

                                    Ok notQuiteLowest ->
                                        { scene =
                                            current.scene
                                                |> ArraySized.elementAlter ( Up, x )
                                                    (ArraySized.elementAlter
                                                        ( Up, notQuiteLowest |> N.sub n1 )
                                                        (\tile ->
                                                            case tile of
                                                                Collidable (Dice ()) ->
                                                                    Air ()

                                                                _ ->
                                                                    tile
                                                        )
                                                    )
                                        , flip =
                                            current.flip
                                                |> (case
                                                        current.scene
                                                            |> ArraySized.element ( Up, x )
                                                            |> ArraySized.element
                                                                ( Up, notQuiteLowest |> N.minSub n1 )
                                                    of
                                                        Collidable (Dice ()) ->
                                                            not

                                                        _ ->
                                                            identity
                                                   )
                                        }

                            ( _, _ ) ->
                                { scene = current.scene
                                , flip = current.flip
                                }
                in
                Step.to
                    { current
                        | playerWarmUp = updatedMoveY.warmUp
                        , playerPosition =
                            if isBelowScene then
                                Point2d.fromRecord Pixels.float { x = 0, y = 37 }

                            else
                                current.playerPosition
                                    |> (case willCollideBelow of
                                            Nothing ->
                                                (case willCollideLeft of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        point2dMapX
                                                            (Quantity.floor
                                                                >> Quantity.toFloatQuantity
                                                                >> Quantity.plus (Pixels.float 0.5)
                                                            )
                                                )
                                                    >> (case willCollideRight of
                                                            Nothing ->
                                                                identity

                                                            Just _ ->
                                                                point2dMapX
                                                                    (Quantity.ceiling
                                                                        >> Quantity.toFloatQuantity
                                                                        >> Quantity.minus (Pixels.float 0.5)
                                                                    )
                                                       )

                                            Just _ ->
                                                point2dMapY
                                                    (Quantity.ceiling
                                                        >> Quantity.toFloatQuantity
                                                        >> Quantity.minus (Pixels.float 0.5)
                                                    )
                                       )
                                    |> Point2d.translateBy
                                        (playerSpeedUpdated
                                            |> Vector2d.for delta
                                        )
                        , playerSpeed = playerSpeedUpdated
                        , scene = onDice.scene
                        , flip = onDice.flip
                        , keyUpChanges =
                            current.keyUpChanges
                                |> List.filterMap
                                    (\key ->
                                        case [ key ] |> keysXDirection of
                                            Nothing ->
                                                key |> Just

                                            Just _ ->
                                                Nothing
                                    )
                        , keyDownChanges =
                            current.keyDownChanges
                                |> List.filterMap
                                    (\key ->
                                        case [ key ] |> keysXDirection of
                                            Nothing ->
                                                key |> Just

                                            Just _ ->
                                                Nothing
                                    )
                    }


type alias Scene =
    ArraySized
        (Exactly N64)
        (ArraySized
            (Exactly N32)
            Tile
        )


sceneRandom :
    { permutations : Simplex.PermutationTable }
    -> Scene
sceneRandom { permutations } =
    let
        ground =
            groundRandom
                { permutations = permutations }
    in
    sceneAir
        |> ground


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


groundRandom :
    { permutations : Simplex.PermutationTable }
    -> (Scene -> Scene)
groundRandom { permutations } =
    \scene ->
        ArraySized.until n63
            |> ArraySized.foldFrom scene
                Up
                (\x ->
                    let
                        noiseInLayer { scale } =
                            \layer ->
                                noise
                                    { permutations = permutations
                                    , x = x |> N.toFloat
                                    , y = layer
                                    }
                                    * scale
                                    |> round
                                    |> N.intIn ( n0, n31 )
                    in
                    ArraySized.elementAlter ( Up, x )
                        (\airColumn ->
                            let
                                groundYUpper =
                                    0 |> noiseInLayer { scale = 7 }

                                aboveGroundGrassUpper =
                                    3 |> noiseInLayer { scale = 3 }
                            in
                            ArraySized.until groundYUpper
                                |> ArraySized.foldFrom airColumn
                                    Up
                                    (\y ->
                                        ArraySized.elementReplace ( Up, y )
                                            (\() -> Collidable (Ground ()))
                                    )
                                |> (\ground ->
                                        ArraySized.until aboveGroundGrassUpper
                                            |> ArraySized.map (N.addAtLeast n0 groundYUpper)
                                            |> ArraySized.foldFrom ground
                                                Up
                                                (\y ->
                                                    ArraySized.elementReplace ( Up, y )
                                                        (\() -> Collidable (Grass ()))
                                                )
                                   )
                                |> (\groundWithGrass ->
                                        ArraySized.until (10 |> noiseInLayer { scale = 1.4 })
                                            |> ArraySized.map
                                                (N.addAtLeast n0 groundYUpper)
                                            |> ArraySized.foldFrom groundWithGrass
                                                Up
                                                (\y ->
                                                    ArraySized.elementReplace ( Up, y )
                                                        (\() -> Collidable (Shrub ()))
                                                )
                                   )
                                |> ArraySized.elementReplace
                                    ( Up
                                    , 20 |> noiseInLayer { scale = 10 }
                                    )
                                    (\() -> Collidable (Leaf ()))
                                |> (if (20 |> noiseInLayer { scale = 0.8 } |> N.toFloat) > 0.7 then
                                        ArraySized.elementReplace
                                            ( Up
                                            , groundYUpper |> N.add n1
                                            )
                                            (\() -> Collidable (Dice ()))

                                    else
                                        identity
                                   )
                        )
                )


warmUpDuration : Duration
warmUpDuration =
    Duration.seconds 0.2


gravity : Vector2d (Rate (Rate Pixels Seconds) Seconds) coordinates_
gravity =
    Vector2d.pixels 0 -18
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


htmlUi : Model -> Html Event
htmlUi =
    \model ->
        Html.div []
            [ model
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
            ]


ui : Model -> Svg event_
ui =
    \{ playerPosition, playerSpeed, flip, windowWidth, windowHeight, playerWarmUp, scene } ->
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
        , [ scene |> sceneUi
          , playerUi { warmUp = playerWarmUp, speed = playerSpeed }
                |> List.singleton
                |> Svg.g
                    [ SvgA.transform
                        [ translateTo playerPosition ]
                    ]
          ]
            |> Svg.g
                [ let
                    tileSize : Float
                    tileSize =
                        ((windowHeight |> Pixels.toFloat)
                            + (case flip of
                                True ->
                                    20

                                False ->
                                    -20
                              )
                        )
                            * 0.88
                            / 32
                  in
                  SvgA.transform
                    [ Svg.Scale tileSize tileSize
                    , case flip of
                        True ->
                            Svg.Scale 1 -1

                        False ->
                            Svg.Translate 0 0
                    ]
                ]
            |> List.singleton
            |> Svg.g
                [ SvgA.transform
                    [ Svg.Scale 1 -1
                    , translateTo
                        (Point2d.pixels
                            (windowWidth |> Quantity.multiplyBy 0.5 |> Pixels.toFloat)
                            (windowHeight
                                |> Quantity.negate
                                |> Quantity.multiplyBy 0.5
                                |> Pixels.toFloat
                            )
                        )
                    ]
                ]
        , [ Svg.rect
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
        ]
            |> Svg.g
                []


playerUi : { warmUp : Progress, speed : Vector2d (Rate Pixels Seconds) Float } -> Svg event_
playerUi { warmUp, speed } =
    let
        after =
            speed
                |> Vector2d.for (Duration.seconds 0.15)

        stretch =
            Vector2d.unitless 0.37 0.37
                |> Vector2d.plus
                    (after
                        |> Vector2d.normalize
                        |> vector2dMapX Quantity.abs
                        |> vector2dMapY Quantity.abs
                        |> Vector2d.scaleBy
                            (after
                                |> Vector2d.length
                                |> Quantity.min (Pixels.float 0.26)
                                |> Quantity.max (Pixels.float -0.26)
                                |> Pixels.toFloat
                            )
                    )

        beforeY =
            0.5
                + (speed
                    |> Vector2d.for (Duration.seconds -0.1)
                    |> Vector2d.yComponent
                    |> Pixels.toFloat
                    |> min 0.7
                    |> max -0.7
                  )

        bodyColor =
            rgb
                (0.2 + 0.8 * (warmUp |> Progress.toFraction))
                0.4
                (1 - 0.8 * (warmUp |> Progress.toFraction))

        arm { x } =
            Svg.polyline
                [ SvgA.stroke (Svg.Paint bodyColor)
                , SvgA.strokeWidth (Svg.px 0.41)
                , SvgA.points
                    [ ( x * 0.35, 0 )
                    , ( x, 0.2 + beforeY * 0.6 )
                    ]
                ]
                []

        eye { x } =
            Svg.circle
                [ SvgA.fill (Svg.Paint (rgba 0.8 1 0.3 0.4))
                , SvgA.r (Svg.px 0.23)
                , SvgA.cx (Svg.px x)
                , SvgA.cy
                    (Svg.px beforeY)
                ]
                []
    in
    [ arm { x = 0.5 }
    , arm { x = -0.5 }
    , Svg.ellipse
        [ SvgA.fill (Svg.Paint bodyColor)
        , SvgA.rx (Svg.px (stretch |> Vector2d.xComponent |> Quantity.toFloat))
        , SvgA.ry (Svg.px (stretch |> Vector2d.yComponent |> Quantity.toFloat))
        , SvgA.cx (Svg.px 0)
        , SvgA.cy (Svg.px 0)
        ]
        []
    , eye { x = 0.3 }
    , eye { x = -0.3 }
    ]
        |> Svg.g []


sceneUi : Scene -> Svg event_
sceneUi =
    \scene ->
        ArraySized.until n63
            |> ArraySized.map
                (\x ->
                    let
                        column =
                            scene |> ArraySized.element ( Up, x )
                    in
                    ArraySized.until n31
                        |> ArraySized.map
                            (\y ->
                                column
                                    |> ArraySized.element ( Up, y )
                                    |> tileUi
                                    |> List.singleton
                                    |> Svg.g
                                        [ SvgA.transform
                                            [ Svg.Translate (x |> N.toFloat) (y |> N.toFloat) ]
                                        ]
                            )
                )
            |> ArraySized.toList
            |> List.concatMap ArraySized.toList
            |> Svg.g
                [ SvgA.transform
                    [ Svg.Translate -32 -16
                    ]
                ]


tileUi : Tile -> Svg event_
tileUi =
    \tile ->
        case tile of
            Air airSpecific ->
                airUi airSpecific

            Collidable collidable ->
                case collidable of
                    Ground groundSpecific ->
                        groundUi groundSpecific

                    Grass grassSpecific ->
                        grassUi grassSpecific

                    Leaf treeSpecific ->
                        treeUi treeSpecific

                    Trunk trunkSpecific ->
                        trunkUi trunkSpecific

                    Shrub shrubSpecific ->
                        shrubUi shrubSpecific

                    Dice diceSpecific ->
                        diceUi diceSpecific


tile1Color color =
    Svg.rect
        [ SvgA.width (Svg.px 1.03)
        , SvgA.height (Svg.px 1.02)
        , SvgA.fill (Svg.Paint color)
        ]
        []


trunkUi trunkSpecific =
    tile1Color (rgb 0.3 0.46 0)


airUi airSpecific =
    tile1Color (rgba 1 1 1 0)


groundUi groundSpecific =
    tile1Color (rgb 0.1 0.04 0)


grassUi () =
    tile1Color (rgb 0.2 0.7 0)


treeUi () =
    tile1Color (rgb 0.099 0.16 0.44)


shrubUi () =
    tile1Color (rgb 0.099 0.128 0.054)


diceUi () =
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



--


vector2dMapX :
    (Quantity Float units -> Quantity Float units)
    ->
        (Vector2d units coordinates
         -> Vector2d units mappedCoordinates_
        )
vector2dMapX xChange =
    \vector ->
        Vector2d.xy
            (vector
                |> Vector2d.xComponent
                |> xChange
            )
            (vector |> Vector2d.yComponent)


vector2dMapY :
    (Quantity Float units -> Quantity Float units)
    ->
        (Vector2d units coordinates
         -> Vector2d units mappedCoordinates_
        )
vector2dMapY yChange =
    \vector ->
        Vector2d.xy
            (vector |> Vector2d.xComponent)
            (vector
                |> Vector2d.yComponent
                |> yChange
            )


point2dMapY :
    (Quantity Float units -> Quantity Float units)
    ->
        (Point2d units coordinates
         -> Point2d units mappedCoordinates_
        )
point2dMapY yChange =
    \point ->
        Point2d.xy
            (point |> Point2d.xCoordinate)
            (point
                |> Point2d.yCoordinate
                |> yChange
            )


point2dMapX :
    (Quantity Float units -> Quantity Float units)
    ->
        (Point2d units coordinates
         -> Point2d units mappedCoordinates_
        )
point2dMapX xChange =
    \point ->
        Point2d.xy
            (point
                |> Point2d.xCoordinate
                |> xChange
            )
            (point |> Point2d.yCoordinate)


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
