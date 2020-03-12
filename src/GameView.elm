module GameView exposing (..)

import Array exposing (Array)
import GameLogic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (disabled, src, style, width)
import Html.Events exposing (..)
import Maybe.Extra
import Model exposing (..)


cardSize =
    ( 120, 189 )


viewCard : Card -> Bool -> Bool -> List (Attribute Msg) -> Html Msg
viewCard card highlighted showBack onClickHandler =
    let
        colorAttributes =
            case highlighted of
                True ->
                    [ style "background-color" "#73D216" ]

                False ->
                    []

        cardText =
            case showBack of
                True ->
                    "X"

                False ->
                    Debug.toString card

        isDisabled =
            case List.isEmpty onClickHandler of
                True ->
                    [ disabled True ]

                False ->
                    []
    in
    button (onClickHandler ++ isDisabled ++ colorAttributes) [ text cardText ]


viewCardImage : Card -> Bool -> Bool -> List (Attribute Msg) -> Html Msg
viewCardImage card highlighted showBack onClickHandler =
    let
        highlightedAttributes =
            if highlighted then
                [ style "border" "4px solid green", style "border-radius" "20px" ]

            else
                []

        imageFileName =
            if showBack then
                "-back"

            else
                case card of
                    Speed speed ->
                        "" ++ String.fromInt speed

                    Minus50 ->
                        "-50"

                    ServiceStation ->
                        "-service"

                    DrawCard numberCardsToDraw ->
                        "+" ++ String.fromInt numberCardsToDraw

                    Discard ->
                        "-discard"
    in
    img ([ src ("images/card" ++ imageFileName ++ ".png"), width (Tuple.first cardSize) ] ++ highlightedAttributes ++ onClickHandler) []


emptyCardView handlers =
    let
        color =
            if List.isEmpty handlers then
                "#aaa"

            else
                "black"
    in
    div
        ([ style "width" ((cardSize |> Tuple.first |> String.fromInt) ++ "px")
         , style "height" ((cardSize |> Tuple.second |> String.fromInt) ++ "px")
         , style "border" ("4px dashed " ++ color)
         , style "border-radius" "20px"
         , style "display" "inline-table"
         ]
            ++ handlers
        )
        [ div
            [ style "display" "table-cell"
            , style "vertical-align" "middle"
            , style "text-align" "center"
            , style "font-size" "64px"
            , style "font-weight" "900"
            , style "color" color
            ]
            [ text "✚" ]
        ]


viewPlayer : GameInfo -> Int -> Player -> Html Msg
viewPlayer gameInfo playerIndex player =
    let
        isPlayerInTurn =
            gameInfo.roundState == PlayerInTurn playerIndex || gameInfo.roundState == RevealSharedPileCardPlayerInTurn playerIndex

        maxNumberCardsToRoute =
            if gameInfo.roundState == PlayerInTurn playerIndex then
                1

            else if gameInfo.roundState == RevealSharedPileCardPlayerInTurn playerIndex then
                case gameInfo.sharedPileCard of
                    Just (DrawCard numberCardsToDraw) ->
                        numberCardsToDraw

                    _ ->
                        0

            else
                0

        handCards =
            div []
                (Array.toList
                    (Array.indexedMap
                        (\cardIndex card ->
                            if isPlayerInTurn then
                                viewCardImage card (Just cardIndex == player.selectedHandCardIndex) False [ onClick (HandCardClicked playerIndex cardIndex) ]

                            else
                                viewCardImage card False True []
                        )
                        player.hand
                    )
                )

        lastToRouteCard =
            Array.get (Array.length player.cardsToRoute - 1) player.cardsToRoute

        routeCardsHidden =
            gameInfo.roundState == StageEnd || isPlayerInTurn |> not

        routeCards =
            div []
                ([ text "Route: " ]
                    ++ (Array.toList (Array.map (\card -> viewCardImage card False routeCardsHidden []) player.route)
                            ++ Array.toList (Array.map (\card -> viewCardImage card False False []) player.cardsToRoute)
                            ++ [ if Array.length player.cardsToRoute < maxNumberCardsToRoute then
                                    emptyCardView <| Maybe.Extra.unwrap [] (\index -> [ onClick <| AddToRouteClicked playerIndex index ]) player.selectedHandCardIndex

                                 else
                                    text ""
                               ]
                            ++ [ if Array.length player.cardsToRoute + 1 < maxNumberCardsToRoute then
                                    emptyCardView []

                                 else
                                    text ""
                               ]
                            ++ [ if isPlayerInTurn then
                                    Maybe.Extra.unwrap (text "") (\card -> button [ onClick <| TakeRouteCardBackClicked playerIndex card ] [ text "Take Card Back" ]) lastToRouteCard

                                 else
                                    text ""
                               ]
                       )
                )
    in
    div []
        [ text (player.name ++ ": ")
        , handCards
        , routeCards
        ]


viewSharedPile : GameInfo -> Html Msg
viewSharedPile gameInfo =
    let
        sharedPile =
            Array.toList (Array.map (\card -> viewCardImage card False True []) gameInfo.sharedPile)

        sharedPileCard =
            Maybe.Extra.unwrap [] (\card -> [ viewCardImage card False False [] ]) gameInfo.sharedPileCard

        playerInTurnToPlaceSharedPileCard =
            case gameInfo.roundState of
                PlayerInTurn playerIndex ->
                    Just playerIndex

                _ ->
                    Nothing
    in
    div []
        ([ text "Shared Pile:"
         , div [] [ text <| "Sum: " ++ String.fromInt gameInfo.sharedPileSum ]
         ]
            ++ sharedPile
            ++ sharedPileCard
            ++ (case playerInTurnToPlaceSharedPileCard of
                    Just playerIndex ->
                        case gameInfo.sharedPileCard of
                            Just card ->
                                [ button [ onClick <| TakeSharedPileCardBackClicked playerIndex card ] [ text "Take Card Back" ] ]

                            Nothing ->
                                [ emptyCardView
                                    (if getSelectedCardFromPlayersHand playerIndex gameInfo.players == Nothing then
                                        []

                                     else
                                        [ onClick <| AddToSharedPileClicked playerIndex ]
                                    )
                                ]

                    Nothing ->
                        []
               )
        )


viewRoundStateButton : GameInfo -> Html Msg
viewRoundStateButton gameInfo =
    let
        startButton playerIndex =
            button [ onClick <| StartTurnClicked ] [ text <| "Start " ++ Maybe.Extra.unwrap "" (\player -> player.name) (Array.get playerIndex gameInfo.players) ++ "'s Turn" ]
    in
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            startButton playerIndex

        PlayerInTurn playerIndex ->
            let
                isButtonDisabled =
                    Maybe.Extra.unwrap True (\player -> Array.isEmpty player.cardsToRoute || Maybe.Extra.isNothing gameInfo.sharedPileCard) (Array.get playerIndex gameInfo.players)
            in
            button [ disabled isButtonDisabled, onClick <| EndTurnClicked ] [ text "End Turn" ]

        RevealSharedPileCard ->
            let
                nextSharedPileCard =
                    Array.get (Array.length gameInfo.sharedPile - 1) gameInfo.sharedPile
            in
            if nextSharedPileCard == Nothing then
                button [ onClick <| RevealSharedPileCardClicked ]
                    [ if isStageEnd gameInfo then
                        text "End Stage"

                      else
                        text "Next Round"
                    ]

            else
                button [ onClick <| RevealSharedPileCardClicked ] [ text "Reveal Shared Pile Card" ]

        RevealSharedPileCardNextPlayerInTurn playerIndex ->
            startButton playerIndex

        RevealSharedPileCardPlayerInTurn playerIndex ->
            case gameInfo.sharedPileCard of
                Just (DrawCard numberCardsToDraw) ->
                    let
                        isButtonDisabled =
                            Maybe.Extra.unwrap True (\player -> Array.length player.cardsToRoute /= numberCardsToDraw) (Array.get playerIndex gameInfo.players)
                    in
                    button [ disabled isButtonDisabled, onClick <| EndTurnClicked ] [ text "End Turn" ]

                _ ->
                    button [ onClick <| RevealSharedPileCardClicked ] [ text "Reveal Shared Pile Card" ]

        StageEnd ->
            if gameInfo.stageNumber >= 4 then
                button [ onClick EndGameClicked ] [ text "End Game" ]

            else
                button [ onClick NextStageClicked ] [ text "Next Stage" ]


viewGameStats : GameInfo -> Html Msg
viewGameStats gameInfo =
    div []
        [ text "Score: "
        , gameInfo.players
            |> Array.map (\player -> div [] [ player.name ++ ": " ++ String.fromInt player.score |> text ])
            |> Array.toList
            |> div []
        ]


viewGame : GameInfo -> Html Msg
viewGame gameInfo =
    div []
        [ Array.indexedMap (viewPlayer gameInfo) gameInfo.players
            |> Array.toList
            |> List.intersperse (div [] [ br [] [] ])
            |> div []
        , div [] [ br [] [] ]
        , viewSharedPile gameInfo
        , div [] [ br [] [] ]
        , viewRoundStateButton gameInfo
        , div [] [ br [] [] ]
        , viewGameStats gameInfo
        ]
