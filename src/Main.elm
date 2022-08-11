port module Main exposing (main)

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
import Array
import ArraySized exposing (ArraySized)
import Audio exposing (Audio, AudioCmd, AudioData, audioDefaultConfig, elementWithAudio)
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
import Json.Decode
import Json.Encode
import Keyboard exposing (Key, KeyChange(..))
import Keyboard.Arrows
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Linear exposing (DirectionLinear(..))
import List.Extra as List
import List.Linear
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
import Result.Extra as Result
import Simplex
import Speed
import Stack
import Svg.Keyed
import Svg.Lazy as Svg
import Svg.PathD
import Task
import Time
import TypedSvg as Svg exposing (svg)
import TypedSvg.Attributes as SvgA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Filters as SvgFilter
import TypedSvg.Filters.Attributes as SvgFilterA
import TypedSvg.Types as Svg exposing (Paint)
import Vector2d exposing (Vector2d)


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
        , playerPosition : Point2d Pixels Float
        , playerSpeed : Vector2d (Rate Pixels Seconds) Float
        , scene : Scene
        , windowWidth : Quantity Float Pixels
        , windowHeight : Quantity Float Pixels
        , keysPressed : List Key
        , keyUpChanges : List Key
        , keyDownChanges : List Key
        , digBlockSound : Result (ProcessError Audio.LoadError) Audio.Source
        , digTimes : List { time : Time.Posix, tile : TileCollidable }

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


type ProcessError loadError
    = Loading
    | LoadError loadError


type alias Scene =
    ArraySized
        (Exactly N64)
        (ArraySized
            (Exactly N32)
            Tile
        )


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


type Shovel
    = Block
    | Circle
    | Line


type Event
    = WindowSized
        { width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
        State
    | KeyEvent Keyboard.Msg State
    | DigSoundLoaded
        { kind : Shovel
        , result : Result Audio.LoadError Audio.Source
        }
    | AnimationFramePassed { delta : Duration } State
    | PermutationsGenerated Simplex.PermutationTable State
    | Dug { time : Time.Posix, tile : TileCollidable }


sceneAir : Scene
sceneAir =
    ArraySized.repeat
        (ArraySized.repeat (Air ()) n32)
        n64


init : () -> AppStep State Event
init () =
    AppStep.to
        { seed =
            -- dummy. Will be replaced
            Random.initialSeed 1329952631
        , permutations =
            -- dummy. Will be replaced
            Simplex.permutationTableFromInt 1329952631
        , playerPosition = Point2d.pixels 0 0
        , playerSpeed =
            (Vector2d.fromRecord Pixels.float
                >> Vector2d.per Duration.second
            )
                { x = 0, y = 0 }
        , scene = sceneAir
        , windowWidth = Pixels.pixels 1920
        , windowHeight = Pixels.pixels 1080
        , keysPressed = []
        , keyUpChanges = []
        , keyDownChanges = []
        , digBlockSound = Err Loading
        , digTimes = []
        , upside = Up
        , lives = 1
        , shovel = Block
        }
        |> AppStep.withCommand
            (Browser.Dom.getViewport
                |> Task.perform
                    (\{ viewport } ->
                        WindowSized
                            { width = Pixels.pixels viewport.width
                            , height = Pixels.pixels viewport.height
                            }
                    )
            )
        |> AppStep.withAudioCommand
            (Audio.loadAudio
                (\result _ ->
                    DigSoundLoaded { kind = Block, result = result }
                )
                "https://cors-anywhere.herokuapp.com/https://freepd.com/music/Wakka%20Wakka.mp3"
             --"https://github.com/lue-bird/dice-digging/blob/master/src/audio/dig-block.mp3"
            )


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : AudioData -> State -> Sub Event
subscriptions audioData =
    subscribeTo
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


subscribeTo : List (Sub (state -> event)) -> state -> Sub event
subscribeTo subscriptions_ =
    \state ->
        subscriptions_
            |> List.map (Sub.map (\f -> f state))
            |> Sub.batch


type XDirection
    = Left
    | Right


reactTo : Event -> AudioData -> State -> AppStep State Event
reactTo event _ deprecatedState =
    case event of
        WindowSized size model ->
            AppStep.to
                { model
                    | windowWidth = size.width |> Quantity.plus (Pixels.float 16)
                    , windowHeight = size.height |> Quantity.plus (Pixels.float 16)
                }
                |> AppStep.withCommand
                    (Simplex.permutationTableGenerator
                        |> Random.generate PermutationsGenerated
                    )

        KeyEvent keyEvent model ->
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
            AppStep.to
                { keyChangesUpdated
                    | keysPressed = keysPressedUpdated
                }

        PermutationsGenerated permutations model ->
            AppStep.to
                { model
                    | permutations = permutations
                    , scene =
                        sceneRandom
                            { permutations = permutations }
                }

        DigSoundLoaded digSound ->
            AppStep.to
                { deprecatedState
                    | digBlockSound =
                        digSound.result
                            |> Result.mapError LoadError
                }

        AnimationFramePassed { delta } current ->
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

                        ( ( Err _, _ ), _ ) ->
                            Nothing

                        ( ( Ok _, Err _ ), _ ) ->
                            Nothing

                        ( ( Ok _, Ok _ ), Err _ ) ->
                            Nothing

                playerIndexInSceneCurrent =
                    current.playerPosition |> playerPositionIndexInScene

                --xCollide : Never -> Maybe (Maybe TileCollidable)
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

                        ( Err _, _ ) ->
                            Nothing

                        ( Ok _, Err _ ) ->
                            Nothing

                updatedMoveY :
                    Vector2d (Rate Pixels Seconds) Float
                    -> Vector2d (Rate Pixels Seconds) Float
                updatedMoveY =
                    case current.keysPressed |> keysYDirection of
                        Just Down ->
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

                        _ ->
                            case belowCollide of
                                Nothing ->
                                    identity

                                Just _ ->
                                    Vector2d.plus
                                        (Vector2d.pixels 0 8
                                            |> Vector2d.per Duration.second
                                        )

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
                                            ((Pixels.float
                                                >> Quantity.per Duration.second
                                             )
                                                (case direction of
                                                    Left ->
                                                        -1.0

                                                    Right ->
                                                        1.0
                                                )
                                            )
                                        )
                                        >> vector2dMapX
                                            (Quantity.clamp
                                                (Pixels.float -12 |> Quantity.per Duration.second)
                                                (Pixels.float 12 |> Quantity.per Duration.second)
                                            )
                           )
                        |> updatedMoveY
            in
            AppStep.to current
                |> AppStep.alter (\r -> { r | playerSpeed = playerSpeedUpdated })
                |> AppStep.alter
                    (if willBeBelowScene then
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
                |> AppStep.alter
                    (\r ->
                        let
                            dig : { scene : Scene, dice : Int }
                            dig =
                                case belowCollide of
                                    Just belowCollision ->
                                        let
                                            consTry =
                                                \( x, y ) ->
                                                    Ok (\xOk yOk -> (::) ( xOk, yOk ))
                                                        |> Result.andMap x
                                                        |> Result.andMap y
                                                        |> Result.withDefault identity

                                            shoveled =
                                                []
                                                    |> consTry
                                                        ( belowCollision.x |> Ok
                                                        , belowCollision.y |> Ok
                                                        )
                                                    |> (case current.shovel of
                                                            Block ->
                                                                identity

                                                            Line ->
                                                                consTry
                                                                    ( belowCollision.x |> Ok
                                                                    , belowCollision.y |> N.isAtLeast n1 |> Result.map (N.minSub n1)
                                                                    )
                                                                    >> consTry
                                                                        ( belowCollision.x |> Ok
                                                                        , belowCollision.y |> N.isAtLeast n2 |> Result.map (N.minSub n2)
                                                                        )

                                                            Circle ->
                                                                consTry
                                                                    ( belowCollision.x |> N.isAtLeast n1 |> Result.map (N.minSub n1)
                                                                    , belowCollision.y |> Ok
                                                                    )
                                                                    >> consTry
                                                                        ( belowCollision.x |> Ok
                                                                        , belowCollision.y |> N.isAtLeast n1 |> Result.map (N.minSub n1)
                                                                        )
                                                                    >> consTry
                                                                        ( belowCollision.x |> N.add n1 |> N.minDown n1 |> N.isAtMost n63
                                                                        , belowCollision.y |> Ok
                                                                        )
                                                       )
                                        in
                                        { scene =
                                            shoveled
                                                |> List.Linear.foldFrom
                                                    ( current.scene
                                                    , Up
                                                    , \( x, y ) ->
                                                        ArraySized.elementAlter ( Up, x )
                                                            (ArraySized.elementAlter ( Up, y )
                                                                (\_ -> Air ())
                                                            )
                                                    )
                                        , dice =
                                            shoveled
                                                |> List.map
                                                    (\( x, y ) ->
                                                        case
                                                            current.scene
                                                                |> ArraySized.element ( Up, x )
                                                                |> ArraySized.element ( Up, y )
                                                        of
                                                            Collidable (Dice ()) ->
                                                                1

                                                            _ ->
                                                                0
                                                    )
                                                |> List.sum
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
                                            \s -> { s | lives = s.lives + 1 }

                                        Flip ->
                                            \s -> { s | upside = s.upside |> Linear.opposite }

                                        ShovelSwap shovelNew ->
                                            \s -> { s | shovel = shovelNew }
                                )
                                { r
                                    | seed = seedNew
                                    , scene = dig.scene
                                }
                    )
                |> (case belowCollide of
                        Nothing ->
                            identity

                        Just collision ->
                            AppStep.withCommand
                                (Time.now
                                    |> Task.perform
                                        (\now _ ->
                                            Dug { time = now, tile = collision.tile }
                                        )
                                )
                   )

        Dug collision ->
            AppStep.to
                { deprecatedState
                    | digTimes =
                        deprecatedState.digTimes |> (::) collision
                }


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
    Vector2d.pixels 0 -48
        |> Vector2d.per Duration.second
        |> Vector2d.per Duration.second


xIndices =
    N.until n63


yIndices =
    N.until n31



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
                    ((state.windowHeight |> Pixels.toFloat) - 10)
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


ui : State -> Svg event_
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
                    |> Vector2d.for (Duration.seconds 0.01)

            stretchY =
                after
                    |> Vector2d.yComponent
                    |> Quantity.abs
                    |> Quantity.negate
                    |> Quantity.min (Pixels.float 0.12)

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
                Pixels.float 0.8
                    |> Quantity.plus stretchY
                    |> Pixels.toFloat

            width =
                Pixels.float 0.38
                    |> Quantity.minus (stretchY |> Quantity.multiplyBy 0.7)
                    |> Pixels.toFloat
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



-- audio


audio : AudioData -> State -> Audio
audio _ =
    \state ->
        state.digTimes
            |> List.map
                (\{ time } ->
                    state.digBlockSound
                        |> Result.map
                            (\source ->
                                Audio.audioWithConfig
                                    { audioDefaultConfig
                                        | playbackRate = 1.1
                                    }
                                    source
                                    time
                                    |> Audio.offsetBy (Duration.seconds 1)
                            )
                        |> Result.withDefault Audio.silence
                )
            |> Audio.group



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
