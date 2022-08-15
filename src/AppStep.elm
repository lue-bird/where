module AppStep exposing
    ( AppStep
    , to
    , map, eventMap
    , commandsAdd, audioCommandsAdd
    , audioCommand, command, state
    , toTuple
    )

{-|

@docs AppStep


## create

@docs to


## alter

@docs map, eventMap
@docs commandsAdd, audioCommandsAdd


## scan

@docs audioCommand, command, state


## transform

@docs toTuple

-}

import Audio exposing (AudioCmd)


type alias AppStep state event =
    { state : state
    , commands : List (Cmd event)
    , audioCommands : List (AudioCmd event)
    }


state : AppStep state event_ -> state
state =
    .state


{-| All [`commands`](#commands) `batch`ed
-}
command : AppStep state event -> Cmd event
command =
    commands >> Cmd.batch


commands : AppStep state event -> List (Cmd event)
commands =
    .commands


{-| All [`audioCommands`](#audioCommands) batched
-}
audioCommand : AppStep state event -> AudioCmd event
audioCommand =
    audioCommands >> Audio.cmdBatch


audioCommands : AppStep state event -> List (AudioCmd event)
audioCommands =
    .audioCommands


to : state -> AppStep state event_
to stateAltered =
    { state = stateAltered
    , commands = []
    , audioCommands = []
    }


map :
    (state -> stateMapped)
    ->
        (AppStep state event
         -> AppStep stateMapped event
        )
map stateMap =
    \step ->
        to (step |> state |> stateMap)
            |> commandsAdd (step |> commands)
            |> audioCommandsAdd (step |> audioCommands)


{-| Transform the events produced by each command and audio command.
Very similar to `Svg`/.../[`Html.map`](https://guide.elm-lang.org/webapps/structure.html).

Rarely useful in well-structured code,
so definitely read the [section on structure in the guide](https://guide.elm-lang.org/webapps/structure.html) before reaching for this!

-}
eventMap :
    (event -> eventMapped)
    ->
        (AppStep state event
         -> AppStep state eventMapped
        )
eventMap eventChange =
    \step ->
        to (step |> state)
            |> commandsAdd
                (step
                    |> commands
                    |> List.map (Cmd.map eventChange)
                )
            |> audioCommandsAdd
                (step
                    |> audioCommands
                    |> List.map (Audio.cmdMap eventChange)
                )


audioCommandsAdd :
    List (AudioCmd event)
    -> (AppStep state event -> AppStep state event)
audioCommandsAdd audioCommandAdditional =
    \step ->
        { state = step |> state
        , commands = step |> commands
        , audioCommands =
            (step |> audioCommands)
                ++ audioCommandAdditional
        }


commandsAdd :
    List (Cmd event)
    -> (AppStep state event -> AppStep state event)
commandsAdd commandAdditional =
    \step ->
        { state = step |> state
        , commands =
            (step |> commands)
                ++ commandAdditional
        , audioCommands = step |> audioCommands
        }


{-| Last step before giving your init/update functions to the audio app:
3-tuple from

  - [`state`](#state)
  - [`command`](#command)
  - [`audioCommand`](#audioCommand)

-}
toTuple :
    AppStep state event
    ->
        ( state
        , Cmd event
        , AudioCmd event
        )
toTuple =
    \step ->
        ( step |> state
        , step |> command
        , step |> audioCommand
        )
