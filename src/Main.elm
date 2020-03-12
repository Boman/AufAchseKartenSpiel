port module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Bool.Extra
import Browser
import GameLogic exposing (..)
import GameView exposing (..)
import Html exposing (..)
import Maybe.Extra
import Model exposing (..)
import Random exposing (Seed)
import StartView exposing (..)
import Time exposing (Posix)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model <| Start <| StartInfo [ ( "Player 1", False ) ], Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( updateStartInfo
                (\startInfo ->
                    StartInfo (startInfo.players ++ [ ( "Player " ++ (String.fromInt <| 1 + List.length startInfo.players), False ) ])
                )
                model
            , Cmd.none
            )

        NameChanged playerIndex newName ->
            ( updateStartInfo
                (\startInfo ->
                    StartInfo (List.indexedMap (\index ( name, isAI ) -> Bool.Extra.ifElse ( newName, isAI ) ( name, isAI ) (index == playerIndex)) startInfo.players)
                )
                model
            , Cmd.none
            )

        SetPlayerAI playerIndex newIsAI ->
            ( updateStartInfo
                (\startInfo ->
                    StartInfo (List.indexedMap (\index ( name, isAI ) -> Bool.Extra.ifElse ( name, newIsAI ) ( name, isAI ) (index == playerIndex)) startInfo.players)
                )
                model
            , Cmd.none
            )

        RemovePlayer playerIndex ->
            ( updateStartInfo (\startInfo -> StartInfo (List.take playerIndex startInfo.players ++ List.drop (playerIndex + 1) startInfo.players)) model, Cmd.none )

        StartGame startInfo ->
            ( model, Random.generate (GameStarted startInfo) Random.independentSeed )

        GameStarted startInfo seed ->
            ( Model <| Play <| initGame startInfo seed, Cmd.none )

        StartTurnClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    case gameInfo.roundState of
                        NextPlayerInTurn playerIndex ->
                            { gameInfo | roundState = PlayerInTurn playerIndex }

                        RevealSharedPileCardNextPlayerInTurn playerIndex ->
                            { gameInfo | roundState = RevealSharedPileCardPlayerInTurn playerIndex }

                        _ ->
                            gameInfo
                )
                model
            , Cmd.none
            )

        HandCardClicked playerIndex selectedHandCardIndex ->
            ( updatePlayerInModel playerIndex (\player -> { player | selectedHandCardIndex = Just selectedHandCardIndex }) model, Cmd.none )

        AddToRouteClicked playerIndex selectedHandCardIndex ->
            ( updatePlayerInModel playerIndex
                (\player ->
                    { player
                        | selectedHandCardIndex = Nothing
                        , hand = Array.Extra.removeAt selectedHandCardIndex player.hand
                        , cardsToRoute = Maybe.Extra.unwrap player.cardsToRoute (\card -> Array.push card player.cardsToRoute) (Array.get selectedHandCardIndex player.hand)
                    }
                )
                model
            , Cmd.none
            )

        TakeRouteCardBackClicked playerIndex routeCard ->
            ( updatePlayerInModel playerIndex
                (\player ->
                    { player
                        | hand = Array.push routeCard player.hand
                        , cardsToRoute = Array.slice 0 -1 player.cardsToRoute
                    }
                )
                model
            , Cmd.none
            )

        AddToSharedPileClicked playerIndex ->
            ( model
                |> updateGameInfo (\gameInfo -> { gameInfo | sharedPileCard = getSelectedCardFromPlayersHand playerIndex gameInfo.players })
                |> updatePlayerInModel playerIndex
                    (\player ->
                        { player
                            | selectedHandCardIndex = Nothing
                            , hand =
                                case player.selectedHandCardIndex of
                                    Just cardIndex ->
                                        Array.Extra.removeAt cardIndex player.hand

                                    Nothing ->
                                        player.hand
                        }
                    )
            , Cmd.none
            )

        TakeSharedPileCardBackClicked playerIndex sharedPileCard ->
            ( model
                |> updateGameInfo (\gameInfo -> { gameInfo | sharedPileCard = Nothing })
                |> updatePlayerInModel playerIndex (\player -> { player | hand = Array.push sharedPileCard player.hand })
            , Cmd.none
            )

        EndTurnClicked ->
            ( updateGameInfo endTurn model, Cmd.none )

        RevealSharedPileCardClicked ->
            ( updateGameInfo
                (\gameInfo ->
                    let
                        sharedPileCard =
                            Array.get (Array.length gameInfo.sharedPile - 1) gameInfo.sharedPile

                        playersWithoutCardsToRoute =
                            Array.map (\player -> { player | cardsToRoute = Array.empty }) gameInfo.players
                    in
                    case sharedPileCard of
                        Just (DrawCard numberCardsToDraw) ->
                            { gameInfo
                                | roundState = RevealSharedPileCardNextPlayerInTurn (modBy (Array.length gameInfo.players) gameInfo.roundNumber)
                                , sharedPile = Array.Extra.pop gameInfo.sharedPile
                                , players = playersWithoutCardsToRoute
                                , sharedPileCard = Just (DrawCard numberCardsToDraw)
                            }

                        Just card ->
                            { gameInfo
                                | roundState = RevealSharedPileCard
                                , sharedPile = Array.Extra.pop gameInfo.sharedPile
                                , sharedPileSum = gameInfo.sharedPileSum + getCardValue card
                                , players =
                                    case card of
                                        Discard ->
                                            Array.map
                                                (\player ->
                                                    { player
                                                        | cardsToRoute = Array.slice -1 (Array.length player.route) player.route
                                                        , route = Array.slice 0 -1 player.route
                                                    }
                                                )
                                                playersWithoutCardsToRoute

                                        _ ->
                                            playersWithoutCardsToRoute
                                , sharedPileCard = Just card
                            }

                        Nothing ->
                            endRound { gameInfo | players = playersWithoutCardsToRoute, sharedPileCard = Nothing }
                )
                model
            , Cmd.none
            )

        NextStageClicked ->
            ( model |> updateGameInfo nextStage, Cmd.none )

        EndGameClicked ->
            ( model |> endGame, Cmd.none )

        Tick _ ->
            ( model |> makeAIturn, Cmd.none )



-- SUBSCRIPTIONS


port pouchDB : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        tickInterval =
            250
    in
    Sub.batch [ Time.every tickInterval Tick ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Start startInfo ->
            viewStartInfo startInfo

        Play gameInfo ->
            viewGame gameInfo

        End endInfo ->
            let
                playersSortedByScore =
                    endInfo
                        |> List.sortBy Tuple.second
                        |> List.reverse
            in
            div []
                ([ --button [ playersSortedByScore |> List.map Tuple.first |> StartInfo |> StartGame |> onClick ] [ text "Start New Game" ]
                   div [] [ br [] [] ]
                 ]
                    ++ [ playersSortedByScore
                            |> List.indexedMap (\place ( name, score ) -> div [] [ String.fromInt (place + 1) ++ ". Place (" ++ String.fromInt score ++ " km): " ++ name |> text ])
                            |> div []
                       ]
                )
