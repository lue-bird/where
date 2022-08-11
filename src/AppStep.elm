module AppStep exposing
    ( to
    , alter
    , withCommand, withAudioCommand
    , audioCommand, command, state
    , toTuple
    , AppStep
    )

{-|


## create

@docs to


## alter

@docs alter
@docs withCommand, withAudioCommand


## scan

@docs audioCommand, command, state


## transform

@docs toTuple

-}

import Audio exposing (AudioCmd)


type alias AppStep state event =
    { state : state
    , command : Cmd (state -> event)
    , audioCommand : AudioCmd (state -> event)
    }


state : AppStep state event_ -> state
state =
    .state


command : AppStep state event -> Cmd (state -> event)
command =
    .command


audioCommand : AppStep state event -> AudioCmd (state -> event)
audioCommand =
    .audioCommand


to : state -> AppStep state event_
to stateAltered =
    { state = stateAltered
    , command = Cmd.none
    , audioCommand = Audio.cmdNone
    }


alter : (state -> state) -> AppStep state event -> AppStep state event
alter stateAlter =
    \step ->
        to (step |> state |> stateAlter)
            |> withCommand (step |> command)
            |> withAudioCommand (step |> audioCommand)


withAudioCommand : AudioCmd (state -> event) -> (AppStep state event -> AppStep state event)
withAudioCommand audioCommandAdditional =
    \step ->
        { state = step |> state
        , command = step |> command
        , audioCommand =
            [ step |> audioCommand
            , audioCommandAdditional
            ]
                |> Audio.cmdBatch
        }


withCommand : Cmd (state -> event) -> (AppStep state event -> AppStep state event)
withCommand commandAdditional =
    \step ->
        { state = step |> state
        , command =
            [ step |> command
            , commandAdditional
            ]
                |> Cmd.batch
        , audioCommand = step |> audioCommand
        }


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
        , step
            |> command
            |> Cmd.map (\f -> f (step |> state))
        , step
            |> audioCommand
            |> Audio.cmdMap (\f -> f (step |> state))
        )
