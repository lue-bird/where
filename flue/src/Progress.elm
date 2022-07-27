module Progress exposing (Progress(..), begin, by, toFraction)

import Quantity exposing (Quantity)


type Progress
    = InProgress Float
    | Ready


begin : Progress
begin =
    InProgress 0


toFraction : Progress -> Float
toFraction =
    \progress ->
        case progress of
            Ready ->
                1

            InProgress fraction ->
                fraction


by :
    { delta : Quantity Float unit
    , ready : Quantity Float unit
    }
    -> Progress
    -> Progress
by { delta, ready } =
    \current ->
        case current of
            Ready ->
                Ready

            InProgress fraction ->
                let
                    newFraction =
                        fraction
                            + ((delta |> Quantity.unwrap)
                                / (ready |> Quantity.unwrap)
                              )
                in
                if newFraction >= 1 then
                    Ready

                else
                    InProgress newFraction
