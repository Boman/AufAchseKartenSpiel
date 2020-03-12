module GameLogic exposing (..)

import Array exposing (Array)
import Array.Extra
import Bool.Extra
import Maybe.Extra
import Model exposing (..)
import Random exposing (Seed)


isLastPlayerInRound : GameInfo -> Bool
isLastPlayerInRound gameInfo =
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.stageNumber + gameInfo.roundNumber - 1)

        PlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.stageNumber + gameInfo.roundNumber - 1)

        RevealSharedPileCardNextPlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.stageNumber + gameInfo.roundNumber - 1)

        RevealSharedPileCardPlayerInTurn playerIndex ->
            playerIndex == modBy (Array.length gameInfo.players) (gameInfo.stageNumber + gameInfo.roundNumber - 1)

        _ ->
            False


endTurn : GameInfo -> GameInfo
endTurn gameInfo =
    let
        newGameInfo playerIndex =
            { gameInfo
                | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (playerIndex + 1))
                , players =
                    updatePlayerInPlayers playerIndex
                        (\player2 ->
                            { player2
                                | route = Array.append player2.route player2.cardsToRoute
                                , selectedHandCardIndex = Nothing
                                , cardsToRoute = Array.empty
                            }
                        )
                        gameInfo.players
                , sharedPile =
                    Array.append gameInfo.sharedPile (Maybe.Extra.toArray gameInfo.sharedPileCard)
                , sharedPileCard = Nothing
            }

        newGameInfoSharedPileCard playerIndex =
            { gameInfo
                | roundState =
                    if isLastPlayerInRound gameInfo then
                        RevealSharedPileCard

                    else
                        RevealSharedPileCardNextPlayerInTurn (modBy (Array.length gameInfo.players) (playerIndex + 1))
                , players =
                    updatePlayerInPlayers playerIndex
                        (\player2 ->
                            { player2
                                | route = Array.append player2.route player2.cardsToRoute
                                , selectedHandCardIndex = Nothing
                                , cardsToRoute = Array.empty
                            }
                        )
                        gameInfo.players
            }
    in
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            if isLastPlayerInRound gameInfo then
                startRoundEnd (newGameInfo playerIndex)

            else
                newGameInfo playerIndex

        PlayerInTurn playerIndex ->
            if isLastPlayerInRound gameInfo then
                startRoundEnd (newGameInfo playerIndex)

            else
                newGameInfo playerIndex

        RevealSharedPileCardPlayerInTurn playerIndex ->
            newGameInfoSharedPileCard playerIndex

        _ ->
            gameInfo


startRoundEnd : GameInfo -> GameInfo
startRoundEnd gameInfo =
    { gameInfo | roundState = RevealSharedPileCard }


nextStage : GameInfo -> GameInfo
nextStage gameInfo =
    { gameInfo
        | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (gameInfo.stageNumber + 1))
        , roundNumber = 0
        , players = Array.map (\player -> { player | hand = Array.empty, route = Array.empty }) gameInfo.players
    }
        |> fillPlayersHand


calculateScore : Player -> Player
calculateScore player =
    let
        calculateScoreFromRoute : List Card -> Int
        calculateScoreFromRoute route =
            case route of
                (Speed speed1) :: (Speed speed2) :: rest ->
                    if speed2 < speed1 then
                        speed1 + calculateScoreFromRoute (Speed speed2 :: rest)

                    else
                        speed1

                ServiceStation :: (Speed speed) :: rest ->
                    calculateScoreFromRoute (Speed speed :: rest)

                (Speed speed1) :: ServiceStation :: (Speed speed2) :: rest ->
                    if speed2 < speed1 then
                        speed1 + calculateScoreFromRoute (Speed speed2 :: rest)

                    else
                        speed1

                (Speed speed) :: ServiceStation :: _ ->
                    speed

                (Speed speed) :: Minus50 :: _ ->
                    speed

                (Speed speed) :: (DrawCard _) :: _ ->
                    speed

                (Speed speed) :: Discard :: _ ->
                    speed

                ServiceStation :: _ ->
                    0

                Minus50 :: _ ->
                    0

                (DrawCard _) :: _ ->
                    0

                Discard :: _ ->
                    0

                [ Speed speed ] ->
                    speed

                [] ->
                    0
    in
    { player | score = player.score + calculateScoreFromRoute (player.route |> Array.toList |> List.reverse) }


isStageEnd : GameInfo -> Bool
isStageEnd gameInfo =
    gameInfo.sharedPileSum >= 50 * (1 + Array.length gameInfo.players)


endRound : GameInfo -> GameInfo
endRound gameInfo =
    if isStageEnd gameInfo then
        { gameInfo | roundState = StageEnd, players = Array.map calculateScore gameInfo.players }

    else
        { gameInfo
            | roundState = NextPlayerInTurn (modBy (Array.length gameInfo.players) (gameInfo.stageNumber + gameInfo.roundNumber + 1))
            , roundNumber = gameInfo.roundNumber + 1
        }
            |> fillPlayersHand


endGame : Model -> Model
endGame model =
    case model.gameState of
        Play gameInfo ->
            { model | gameState = End (gameInfo.players |> Array.toList |> List.map (\player -> ( player.name, player.score ))) }

        _ ->
            model


makeAIturn : Model -> Model
makeAIturn model =
    let
        isAI : GameInfo -> Int -> Bool
        isAI gameInfo playerIndex =
            Array.get playerIndex gameInfo.players
                |> Maybe.Extra.unwrap False (\player -> Maybe.Extra.isJust player.ai)

        isAIinTurn : GameInfo -> Bool
        isAIinTurn gameInfo =
            case gameInfo.roundState of
                NextPlayerInTurn playerIndex ->
                    isAI gameInfo playerIndex

                PlayerInTurn playerIndex ->
                    isAI gameInfo playerIndex

                RevealSharedPileCardNextPlayerInTurn playerIndex ->
                    isAI gameInfo playerIndex

                RevealSharedPileCardPlayerInTurn playerIndex ->
                    isAI gameInfo playerIndex

                _ ->
                    False
    in
    case model.gameState of
        Play gameInfo ->
            if isAIinTurn gameInfo then
                executeAI gameInfo |> Play |> Model

            else
                model

        _ ->
            model


executeAI : GameInfo -> GameInfo
executeAI gameInfo =
    let
        addCardsToRoute numberOfCards playerIndex gameInfo2 =
            case numberOfCards of
                0 ->
                    gameInfo2

                _ ->
                    addCardsToRoute (numberOfCards - 1)
                        playerIndex
                        { gameInfo2
                            | players =
                                updatePlayerInPlayers playerIndex
                                    (\player ->
                                        let
                                            ( cardIndex, newAI ) =
                                                case player.ai of
                                                    Just seed ->
                                                        Random.step (Random.int 0 (Array.length player.hand - 1)) seed |> Tuple.mapSecond Just

                                                    Nothing ->
                                                        ( -1, Nothing )
                                        in
                                        { player
                                            | hand = Array.Extra.removeAt cardIndex player.hand
                                            , cardsToRoute = Maybe.Extra.unwrap player.cardsToRoute (\card -> Array.push card player.cardsToRoute) (Array.get cardIndex player.hand)
                                            , ai = newAI
                                        }
                                    )
                                    gameInfo2.players
                        }

        addCardToSharedPile playerIndex gameInfo2 =
            let
                newPlayers =
                    updatePlayerInPlayers playerIndex
                        (\player ->
                            let
                                ( cardIndex, newAI ) =
                                    case player.ai of
                                        Just seed ->
                                            Random.step (Random.int 0 (Array.length player.hand - 1)) seed |> Tuple.mapSecond Just

                                        Nothing ->
                                            ( -1, Nothing )
                            in
                            { player
                                | hand = Array.Extra.removeAt cardIndex player.hand
                                , selectedHandCardIndex = Just cardIndex
                                , ai = newAI
                            }
                        )
                        gameInfo2.players
            in
            { gameInfo2
                | players =
                    updatePlayerInPlayers playerIndex
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
                        newPlayers
                , sharedPileCard = getSelectedCardFromPlayersHand playerIndex newPlayers
            }

        executeInTurn playerIndex =
            gameInfo
                |> addCardsToRoute 1 playerIndex
                |> addCardToSharedPile playerIndex
                |> endTurn

        executeSharedPileCardTurn playerIndex =
            case gameInfo.sharedPileCard of
                Just card ->
                    case card of
                        DrawCard numberCardsToDraw ->
                            gameInfo
                                |> addCardsToRoute numberCardsToDraw playerIndex
                                |> endTurn

                        _ ->
                            gameInfo

                Nothing ->
                    gameInfo
    in
    case gameInfo.roundState of
        NextPlayerInTurn playerIndex ->
            executeInTurn playerIndex

        PlayerInTurn playerIndex ->
            executeInTurn playerIndex

        RevealSharedPileCardNextPlayerInTurn playerIndex ->
            executeSharedPileCardTurn playerIndex

        RevealSharedPileCardPlayerInTurn playerIndex ->
            executeSharedPileCardTurn playerIndex

        _ ->
            gameInfo


getCardValue : Card -> Int
getCardValue card =
    case card of
        Speed speed ->
            speed

        Minus50 ->
            -50

        _ ->
            0


cardGenerator : Random.Generator Card
cardGenerator =
    Random.weighted
        ( 12, ServiceStation )
        [ ( 5, Minus50 )
        , ( 5, DrawCard 1 )
        , ( 2, DrawCard 2 )
        , ( 5, Discard )
        , ( 7, Speed 10 )
        , ( 7, Speed 20 )
        , ( 7, Speed 30 )
        , ( 7, Speed 40 )
        , ( 7, Speed 50 )
        , ( 7, Speed 60 )
        , ( 6, Speed 70 )
        , ( 6, Speed 80 )
        , ( 6, Speed 90 )
        , ( 6, Speed 100 )
        , ( 6, Speed 110 )
        , ( 5, Speed 120 )
        ]


initGame : StartInfo -> Seed -> GameInfo
initGame startInfo seed =
    let
        generateAISeeds =
            Random.list (List.length startInfo.players) Random.independentSeed

        ( aiSeeds, newSeed ) =
            Random.step generateAISeeds seed
                |> Tuple.mapFirst Array.fromList

        players =
            Array.fromList <|
                List.indexedMap
                    (\index ( name, isAI ) ->
                        Player name 0 Array.empty Array.empty Nothing Array.empty (Bool.Extra.ifElse (Array.get index aiSeeds) Nothing isAI)
                    )
                    startInfo.players
    in
    GameInfo players 0 0 (NextPlayerInTurn 0) Array.empty Nothing 0 newSeed
        |> fillPlayersHand


fillPlayersHand : GameInfo -> GameInfo
fillPlayersHand gameInfo =
    let
        numCards =
            10 - Maybe.Extra.unwrap 0 (\player -> Array.length player.hand) (Array.get 0 gameInfo.players)

        createDrawDeck =
            Random.list (numCards * Array.length gameInfo.players) cardGenerator

        ( drawDeck, newSeed ) =
            Random.step createDrawDeck gameInfo.randomnessSeed

        takeCards index deck =
            Array.fromList <| List.take numCards <| List.drop (numCards * index) deck

        newPlayers =
            Array.indexedMap (\index player -> { player | hand = Array.append player.hand (takeCards index drawDeck) }) gameInfo.players
    in
    { gameInfo | players = newPlayers, randomnessSeed = newSeed }
