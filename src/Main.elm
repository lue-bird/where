module Main exposing (main)

{-| GMTK Game Jam 2022

  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-geometry/latest>
  - <https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/>
  - <https://dark.elm.dmy.fr/packages/elm-community/typed-svg/latest/>
  - <https://dark.elm.dmy.fr/packages/Spaxe/svg-pathd/latest/Svg-PathD>
  - <https://dark.elm.dmy.fr/packages/Herteby/simplex-noise/latest/>
  - <https://www.youtube.com/watch?v=CoMHkeAXEUY>
  - <https://dark.elm.dmy.fr/packages/xilnocas/step/latest/Step>
  - elm-optimize-level-2 src/Main.elm --optimize-speed
      - <https://github.com/mdgriffith/elm-optimize-level-2>

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
import Html.Lazy
import Keyboard exposing (Key, KeyChange(..))
import Keyboard.Arrows
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Linear exposing (DirectionLinear(..))
import List.Extra as List
import N exposing (Exactly, Fixed, In, Min, N, N0, n0, n1, n2, n3)
import Ns exposing (N31, N32, N63, N64, n31, n32, n62, n63, n64)
import Pixels exposing (Pixels, PixelsPerSecond)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Progress exposing (Progress(..))
import Quantity exposing (Quantity, Rate, Unitless, in_)
import Random
import Random.Extra as Random
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rectangle2d
import Simplex
import Speed
import Stack
import Step exposing (Step)
import Svg.Keyed
import Svg.Lazy as Svg
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
        , scene : Scene
        , windowWidth : Quantity Float Pixels
        , windowHeight : Quantity Float Pixels
        , keysPressed : List Key
        , keyUpChanges : List Key
        , keyDownChanges : List Key

        -- changed by dice roll
        {-
           - change controls (slippery, speed boost, ...)
           - switched dice positions
           - new terrain, more obstacles (traps, spikes, ...)
           - whether, wind
        -}
        , upside : DirectionLinear
        , lives : Int
        , shovel : Shovel
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
    ArraySized.repeat
        (ArraySized.repeat (Air ()) n32)
        n64


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
      , scene = sceneAir
      , windowWidth = Pixels.pixels 1920
      , windowHeight = Pixels.pixels 1080
      , keysPressed = []
      , keyUpChanges = []
      , keyDownChanges = []
      , upside = Up
      , lives = 1
      , shovel = Block
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
        let
            { x } =
                keys |> Keyboard.Arrows.arrows
        in
        if x == -1 then
            Left |> Just

        else if x == 1 then
            Right |> Just

        else
            Nothing


keysYDirection : List Key -> Maybe DirectionLinear
keysYDirection =
    \keys ->
        let
            { y } =
                keys |> Keyboard.Arrows.arrows
        in
        if y == -1 then
            Down |> Just

        else if y == 1 then
            Up |> Just

        else
            Nothing


playerPositionIndexInScene :
    Point2d Pixels Float
    ->
        { xLeft :
            Result
                (N.BelowOrAbove Int (N (Min (Fixed N64))))
                (N (In (Fixed N0) (Fixed N63)))
        , xRight :
            Result
                (N.BelowOrAbove Int (N (Min (Fixed N64))))
                (N (In (Fixed N0) (Fixed N63)))
        , yLow :
            Result
                (N.BelowOrAbove Int (N (Min (Fixed N32))))
                (N (In (Fixed N0) (Fixed N31)))
        , yHigh :
            Result
                (N.BelowOrAbove Int (N (Min (Fixed N32))))
                (N (In (Fixed N0) (Fixed N31)))
        , yRound :
            Result
                (N.BelowOrAbove Int (N (Min (Fixed N32))))
                (N (In (Fixed N0) (Fixed N31)))
        }
playerPositionIndexInScene =
    \playerPosition ->
        { xLeft =
            (((playerPosition
                |> Point2d.xCoordinate
                |> Pixels.toFloat
              )
                - 0.5
             )
                |> floor
            )
                + 32
                |> N.intIsIn ( n0, n63 )
        , xRight =
            (((playerPosition
                |> Point2d.xCoordinate
                |> Pixels.toFloat
              )
                + 0.5
             )
                |> floor
            )
                + 32
                |> N.intIsIn ( n0, n63 )
        , yLow =
            (((playerPosition
                |> Point2d.yCoordinate
                |> Pixels.toFloat
              )
                - 0.5
             )
                |> floor
            )
                + 16
                |> N.intIsIn ( n0, n31 )
        , yHigh =
            (((playerPosition
                |> Point2d.yCoordinate
                |> Pixels.toFloat
              )
                + 0.5
             )
                |> floor
            )
                + 16
                |> N.intIsIn ( n0, n31 )
        , yRound =
            (((playerPosition
                |> Point2d.yCoordinate
                |> Pixels.toFloat
              )
                - 0.5
             )
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
                        | windowWidth = size.width |> Quantity.plus (Pixels.float 16)
                        , windowHeight = size.height |> Quantity.plus (Pixels.float 16)
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
                    playerIndexInSceneFuture =
                        current.playerPosition
                            |> Point2d.translateBy
                                (current.playerSpeed |> Vector2d.for (Duration.seconds 0.055))
                            |> playerPositionIndexInScene

                    willBeBelowScene =
                        case playerIndexInSceneFuture.yLow of
                            Ok _ ->
                                False

                            Err (N.Above _) ->
                                False

                            Err (N.Below _) ->
                                True

                    belowCollide :
                        Maybe
                            { tile : TileCollidable
                            , x : N (In (Fixed N0) (Fixed N63))
                            , y : N (In (Fixed N0) (Fixed N31))
                            }
                    belowCollide =
                        case
                            ( ( playerIndexInSceneFuture.xLeft, playerIndexInSceneFuture.xRight )
                            , playerIndexInSceneFuture.yLow
                            )
                        of
                            ( ( Ok xLeft, Ok xRight ), Ok y ) ->
                                case
                                    current.scene
                                        |> ArraySized.element ( Up, xLeft )
                                        |> ArraySized.element ( Up, y )
                                of
                                    Collidable collidable ->
                                        { tile = collidable, x = xLeft, y = y }
                                            |> Just

                                    Air _ ->
                                        current.scene
                                            |> ArraySized.element ( Up, xRight )
                                            |> ArraySized.element ( Up, y )
                                            |> tileIsCollidable
                                            |> Maybe.map
                                                (\tile ->
                                                    { tile = tile
                                                    , x = xRight
                                                    , y = y
                                                    }
                                                )

                            ( _, _ ) ->
                                Nothing

                    playerIndexInSceneCurrent =
                        current.playerPosition |> playerPositionIndexInScene

                    xCollide xAccess =
                        case
                            ( playerIndexInSceneFuture |> xAccess
                            , playerIndexInSceneCurrent.yRound
                            )
                        of
                            ( Ok x, Ok yRound ) ->
                                current.scene
                                    |> ArraySized.element ( Up, x )
                                    |> ArraySized.element ( Up, yRound )
                                    |> tileIsCollidable

                            ( _, _ ) ->
                                Nothing

                    updatedMoveY :
                        Vector2d (Rate Pixels Seconds) Float
                        -> Vector2d (Rate Pixels Seconds) Float
                    updatedMoveY =
                        let
                            maybeWarmUp =
                                case current.keysPressed |> keysYDirection of
                                    Just Down ->
                                        Vector2d.plus
                                            (Vector2d.pixels 0 -0.5
                                                |> Vector2d.per Duration.second
                                            )

                                    Just Up ->
                                        -- TODO: enable only on dice roll
                                        Vector2d.plus
                                            (Vector2d.pixels 0 1
                                                |> Vector2d.per Duration.second
                                            )

                                    _ ->
                                        identity

                            tryJump =
                                case belowCollide of
                                    Nothing ->
                                        identity

                                    Just _ ->
                                        Vector2d.plus
                                            (Vector2d.pixels 0 8
                                                |> Vector2d.per Duration.second
                                            )
                        in
                        case current.keysPressed |> keysYDirection of
                            Just Down ->
                                maybeWarmUp

                            _ ->
                                tryJump

                    playerSpeedUpdated : Vector2d (Rate Pixels Seconds) Float
                    playerSpeedUpdated =
                        current.playerSpeed
                            |> (case belowCollide of
                                    Nothing ->
                                        Vector2d.plus
                                            (gravity |> Vector2d.for delta)

                                    Just _ ->
                                        vector2dMapY
                                            (Quantity.abs
                                                >> Quantity.multiplyBy 0.6
                                            )
                               )
                            |> (case xCollide .xLeft of
                                    Nothing ->
                                        identity

                                    Just _ ->
                                        vector2dMapX Quantity.abs
                               )
                            |> (case xCollide .xRight of
                                    Nothing ->
                                        identity

                                    Just _ ->
                                        vector2dMapX
                                            (Quantity.abs
                                                >> Quantity.negate
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
                            |> updatedMoveY
                in
                current
                    |> (\r -> { r | playerSpeed = playerSpeedUpdated })
                    |> (if willBeBelowScene then
                            \r ->
                                { r
                                    | playerPosition =
                                        Point2d.fromRecord Pixels.float { x = 0, y = 32 }
                                    , lives = current.lives - 1
                                }

                        else
                            \r ->
                                { r
                                    | playerPosition =
                                        current.playerPosition
                                            |> (case belowCollide of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        point2dMapY
                                                            (Quantity.ceiling
                                                                >> Quantity.toFloatQuantity
                                                                >> Quantity.minus (Pixels.float 0.5)
                                                            )
                                               )
                                            |> (case xCollide .xLeft of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        point2dMapX
                                                            (Quantity.floor
                                                                >> Quantity.toFloatQuantity
                                                                >> Quantity.plus (Pixels.float 0.5)
                                                            )
                                               )
                                            |> (case xCollide .xRight of
                                                    Nothing ->
                                                        identity

                                                    Just _ ->
                                                        point2dMapX
                                                            (Quantity.ceiling
                                                                >> Quantity.toFloatQuantity
                                                                >> Quantity.minus (Pixels.float 0.5)
                                                            )
                                               )
                                            |> Point2d.translateBy
                                                (playerSpeedUpdated
                                                    |> Vector2d.for delta
                                                )
                                }
                       )
                    |> (\r ->
                            let
                                dig : { scene : Scene, dice : Int }
                                dig =
                                    case belowCollide of
                                        Just belowCollision ->
                                            { scene =
                                                case current.shovel of
                                                    Block ->
                                                        current.scene
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter ( Up, belowCollision.y )
                                                                    (\_ -> Air ())
                                                                )

                                                    Line ->
                                                        current.scene
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter ( Up, belowCollision.y )
                                                                    (\_ -> Air ())
                                                                )
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter
                                                                    ( Up
                                                                    , belowCollision.y |> N.atLeast n1 |> N.sub n1
                                                                    )
                                                                    (\_ -> Air ())
                                                                )
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter
                                                                    ( Up
                                                                    , belowCollision.y |> N.atLeast n2 |> N.sub n2
                                                                    )
                                                                    (\_ -> Air ())
                                                                )

                                                    Circle ->
                                                        current.scene
                                                            |> ArraySized.elementAlter
                                                                ( Up
                                                                , belowCollision.x |> N.atLeast n1 |> N.sub n1
                                                                )
                                                                (ArraySized.elementAlter ( Up, belowCollision.y )
                                                                    (\_ -> Air ())
                                                                )
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter
                                                                    ( Up
                                                                    , belowCollision.y |> N.atLeast n1 |> N.sub n1
                                                                    )
                                                                    (\_ -> Air ())
                                                                )
                                                            |> ArraySized.elementAlter ( Up, belowCollision.x )
                                                                (ArraySized.elementAlter ( Up, belowCollision.y )
                                                                    (\_ -> Air ())
                                                                )
                                                            |> ArraySized.elementAlter
                                                                ( Up
                                                                , belowCollision.x |> N.add n1
                                                                )
                                                                (ArraySized.elementAlter ( Up, belowCollision.y )
                                                                    (\_ -> Air ())
                                                                )
                                            , dice =
                                                case belowCollision.tile of
                                                    Dice () ->
                                                        1

                                                    _ ->
                                                        0
                                            }

                                        Nothing ->
                                            { scene = current.scene
                                            , dice = 0
                                            }

                                ( randomChanges, seedNew ) =
                                    case dig.dice of
                                        0 ->
                                            ( [], current.seed )

                                        digDice1AtLeast ->
                                            Random.step
                                                (Random.list digDice1AtLeast (diceChangeRandom r.shovel))
                                                current.seed
                            in
                            randomChanges
                                |> List.foldl
                                    (\randomChange_ ->
                                        case randomChange_ of
                                            LiveUp ->
                                                \m -> { m | lives = m.lives + 1 }

                                            Flip ->
                                                \m -> { m | upside = m.upside |> Linear.opposite }

                                            ShovelSwap shovelNew ->
                                                \m -> { m | shovel = shovelNew }
                                    )
                                    { r
                                        | seed = seedNew
                                        , scene = dig.scene
                                    }
                       )
                    |> Step.to


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
                                            |> ArraySized.map (N.add groundYUpper)
                                            |> ArraySized.foldFrom ground
                                                Up
                                                (\y ->
                                                    ArraySized.elementReplace ( Up, y )
                                                        (\() -> Collidable (Grass ()))
                                                )
                                   )
                                |> (\groundWithGrass ->
                                        ArraySized.until (10 |> noiseInLayer { scale = 1.4 })
                                            |> ArraySized.map (N.add groundYUpper)
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


type DiceChange
    = Flip
    | LiveUp
    | ShovelSwap Shovel


diceChangeRandom : Shovel -> Random.Generator DiceChange
diceChangeRandom currentShovel =
    Random.frequency
        ( 0.2, Random.constant Flip )
        [ ( 0.2, Random.constant LiveUp )
        , ( 0.5
          , Random.constant ShovelSwap
                |> Random.andMap (shovelRandom currentShovel)
          )
        ]


type Shovel
    = Block
    | Circle
    | Line


shovelRandom : Shovel -> Random.Generator Shovel
shovelRandom currentShovel =
    case currentShovel of
        Block ->
            Random.weighted
                ( 0.6, Circle )
                [ ( 0.4, Line )
                ]

        Circle ->
            Random.weighted
                ( 0.6, Block )
                [ ( 0.4, Line )
                ]

        Line ->
            Random.weighted
                ( 0.6, Block )
                [ ( 0.4, Circle )
                ]


gravity : Vector2d (Rate (Rate Pixels Seconds) Seconds) coordinates_
gravity =
    Vector2d.pixels 0 -18
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


background : Svg event_
background =
    Svg.rect
        [ SvgA.fill (Svg.Reference "background")
        , SvgA.width (Svg.percent 100)
        , SvgA.height (Svg.percent 100)
        ]
        []


svgDefinitions =
    Svg.defs
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


ui : Model -> Svg event_
ui =
    \{ playerPosition, playerSpeed, upside, windowWidth, windowHeight, shovel, scene, lives } ->
        [ svgDefinitions
        , background
        , [ Svg.lazy sceneUi scene
          , { speed = playerSpeed
            , shovel = shovel
            , lives = lives
            }
                |> playerUi
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
                            + (case upside of
                                Down ->
                                    20

                                Up ->
                                    -20
                              )
                        )
                            * 0.88
                            / 32
                  in
                  SvgA.transform
                    [ Svg.Scale tileSize tileSize
                    , case upside of
                        Down ->
                            Svg.Scale 1 -1

                        Up ->
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
        , bars
        ]
            |> Svg.g
                []


playerUi :
    { speed : Vector2d (Rate Pixels Seconds) Float
    , shovel : Shovel
    , lives : Int
    }
    -> Svg event_
playerUi =
    \{ speed, shovel, lives } ->
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
                rgb 0.2 0.4 1

            arm { x } =
                Svg.polyline
                    [ SvgA.stroke (Svg.Paint bodyColor)
                    , SvgA.strokeWidth (Svg.px 0.41)
                    , SvgA.points
                        [ ( x * 0.4, 0 )
                        , ( x, 0.2 + beforeY * 0.85 )
                        ]
                    ]
                    []

            shovelUi : Svg event_
            shovelUi =
                case shovel of
                    Block ->
                        Svg.polygon
                            [ SvgA.points
                                [ ( -0.4, 0.33 )
                                , ( 0, 0 )
                                , ( 0.4, 0.33 )
                                ]
                            , SvgA.fill (Svg.Paint (rgba 0 0 0 0.8))
                            , SvgA.transform
                                [ Svg.Translate 0 -0.55 ]
                            ]
                            []

                    Line ->
                        Svg.polygon
                            [ SvgA.points
                                [ ( -0.13, 0.48 )
                                , ( -0.13, 0 )
                                , ( 0.13, 0 )
                                , ( 0.13, 0.48 )
                                ]
                            , SvgA.transform
                                [ Svg.Translate 0 -0.5 ]
                            , SvgA.fill (Svg.Paint (rgba 0 0 0 0.8))
                            ]
                            []

                    Circle ->
                        Svg.circle
                            [ SvgA.fill (Svg.Paint (rgba 0 0 0 0.8))
                            , SvgA.r (Svg.px 0.35)
                            , SvgA.transform
                                [ Svg.Translate 0 -0.2 ]
                            ]
                            []
        in
        [ arm { x = 0.5 }
        , arm { x = -0.5 }
        , let
            height =
                stretch |> Vector2d.yComponent |> Quantity.toFloat

            width =
                stretch |> Vector2d.xComponent |> Quantity.toFloat
          in
          Svg.ellipse
            [ SvgA.rx (Svg.px width)
            , SvgA.ry (Svg.px height)
            , SvgA.fill (Svg.Paint bodyColor)
            ]
            []
        , let
            eye { x, y } =
                Svg.circle
                    [ SvgA.fill (Svg.Paint (rgba 0.8 1 0.3 0.9))
                    , SvgA.r (Svg.px (0.15 + (0.2 / (lives |> toFloat))))
                    , SvgA.cx (Svg.px x)
                    , SvgA.cy (Svg.px (beforeY * 0.9 + y))
                    ]
                    []
          in
          List.range 0 (lives - 1)
            |> List.map
                (\life ->
                    let
                        toMid =
                            -(((lives - 1) |> toFloat) / 2)
                                + (life |> toFloat)
                    in
                    eye
                        { x = 0.6 * cos (turns (0.25 + (toMid / (lives |> toFloat) * 0.5)))
                        , y = 0.2 * sin (turns (0.25 + (toMid / (lives |> toFloat) * 0.5)))
                        }
                )
            |> Svg.g []
        , shovelUi
        ]
            |> Svg.g []


xIndices =
    N.until n63


yIndices =
    N.until n31


sceneUi : Scene -> Svg event_
sceneUi =
    \scene ->
        xIndices
            |> Stack.toList
            |> List.map
                (\x ->
                    (scene |> ArraySized.element ( Up, x ))
                        |> Svg.lazy
                            (\column ->
                                yIndices
                                    |> Stack.toList
                                    |> List.map
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
                                    |> Svg.g []
                            )
                )
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
        [ SvgA.width (Svg.px 1.026)
        , SvgA.height (Svg.px 1.017)
        , SvgA.fill (Svg.Paint color)
        ]
        []


trunkUi trunkSpecific =
    tile1Color (rgb 0.3 0.46 0)


airUi airSpecific =
    Svg.g [] []


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
