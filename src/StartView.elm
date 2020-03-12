module StartView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewStartInfo : StartInfo -> Html Msg
viewStartInfo startInfo =
    let
        mapPlayersToInput =
            \index ( name, isAI ) ->
                div []
                    [ text (String.fromInt (index + 1) ++ ". Player Name: ")
                    , input [ value name, onInput <| NameChanged index ] []
                    , input [ type_ "checkbox", onClick (SetPlayerAI index (not isAI)), checked isAI ] []
                    , text "is AI opponent"
                    , button [ onClick (RemovePlayer index) ] [ text "Remove Player" ]
                    ]
    in
    div []
        [ div [] <| List.indexedMap mapPlayersToInput startInfo.players
        , button [ onClick AddPlayer ] [ text "+ Add Player" ]
        , div [] [ br [] [] ]
        , button [ onClick <| StartGame startInfo ] [ text "Start Game" ]
        ]
