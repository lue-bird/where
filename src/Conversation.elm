module Conversation exposing
    ( Conversation, PredeterminedOrChoice(..)
    , start
    , choice, predetermined
    , Step, step
    )

{-|

@docs Conversation, PredeterminedOrChoice


## build

@docs start
@docs choice, predetermined


## run

@docs Step, step

-}

import Linear exposing (DirectionLinear(..))
import List.Extra as List
import Scroll exposing (FocusGap, Scroll)
import Stack


{-| Kind of like a graph or state machine,
but connections are not state -> state,
they conditionally take a state and transform it.
-}
type Conversation state speaker lines
    = Conversation
        { choice :
            List
                (state
                 ->
                    Maybe
                        { possibilities :
                            List
                                { lines : lines
                                , state : state
                                }
                        , speaker : speaker
                        }
                )
        , predetermined :
            List
                (state
                 ->
                    Maybe
                        { possibilities :
                            Scroll lines FocusGap Never
                        , speaker : speaker
                        , state : state
                        }
                )
        }



-- create


start : Conversation state speaker_ lines_
start =
    Conversation
        { choice = []
        , predetermined = []
        }


choice :
    (state -> Maybe accepted)
    -> (state -> accepted -> ( speaker, List ( lines, state ) ))
    ->
        (Conversation state speaker lines
         -> Conversation state speaker lines
        )
choice stateConditionBeforeTelling tell =
    \(Conversation conversation) ->
        { conversation
            | choice =
                conversation.choice
                    |> (::)
                        (\state ->
                            state
                                |> stateConditionBeforeTelling
                                |> Maybe.map
                                    (\accepted ->
                                        let
                                            ( speaker, told ) =
                                                accepted |> tell state
                                        in
                                        { speaker = speaker
                                        , possibilities =
                                            told
                                                |> List.map
                                                    (\( lines, stateAfter ) ->
                                                        { lines = lines
                                                        , state = stateAfter
                                                        }
                                                    )
                                        }
                                    )
                        )
        }
            |> Conversation


predetermined :
    (state -> Maybe accepted)
    ->
        (state
         -> accepted
         ->
            ( speaker
            , ( List lines, lines, List lines )
            , state
            )
        )
    ->
        (Conversation state speaker lines
         -> Conversation state speaker lines
        )
predetermined stateConditionBeforeTelling tell =
    \(Conversation conversation) ->
        { conversation
            | predetermined =
                conversation.predetermined
                    |> (::)
                        (\state ->
                            state
                                |> stateConditionBeforeTelling
                                |> Maybe.map
                                    (\accepted ->
                                        let
                                            ( speaker, ( possibilitiesBeforeChosen, possibilityChosen, possibilitiesAfterChosen ), stateAfter ) =
                                                accepted |> tell state
                                        in
                                        { speaker = speaker
                                        , state = stateAfter
                                        , possibilities =
                                            Scroll.only possibilityChosen
                                                |> Scroll.sideAlter ( Down, \_ -> possibilitiesBeforeChosen |> Stack.fromList |> Stack.reverse )
                                                |> Scroll.sideAlter ( Up, \_ -> possibilitiesAfterChosen |> Stack.fromList )
                                        }
                                    )
                        )
        }
            |> Conversation



-- step


{-| `Choice`/`Predetermined` differentiation for a different structure than [`Step`](#Step)

    PredeterminedOrChoice () ()

-}
type PredeterminedOrChoice predetermined choice
    = Predetermined predetermined
    | Choice choice


{-| -}
type alias Step state speaker lines =
    PredeterminedOrChoice
        { state : state
        , speaker : speaker
        , possibilities : Scroll lines FocusGap Never
        }
        (List
            { speaker : speaker
            , possibilities : List { lines : lines, state : state }
            }
        )


{-| Play out what's available as the next step in the conversation:

  - the [`choice`](#choice) [`Conversation`](#Conversation)
  - the [`predetermined`](#predetermined) [`Conversation`](#Conversation)

-}
step :
    Conversation state speaker lines
    -> state
    -> Step state speaker lines
step conversation =
    \state ->
        let
            (Conversation conversionParts) =
                conversation
        in
        case
            conversionParts.predetermined
                |> List.findMap
                    (\try -> state |> try)
        of
            Just predeterminedStepped ->
                predeterminedStepped |> Predetermined

            Nothing ->
                conversionParts.choice
                    |> List.filterMap
                        (\try -> state |> try)
                    |> Choice
