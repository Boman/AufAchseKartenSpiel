module GameLogic exposing (..)

import Array exposing (Array)
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


endTurn : Int -> GameInfo -> GameInfo
endTurn playerIndex gameInfo =
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
                [] ->
                    0

                [ Speed speed ] ->
                    speed

                (Speed speed1) :: (Speed speed2) :: rest ->
                    if speed2 < speed1 then
                        speed1 + calculateScoreFromRoute (Speed speed2 :: rest)

                    else
                        speed1

                ServiceStation :: (Speed speed) :: rest ->
                    calculateScoreFromRoute (Speed speed :: rest)

                (Speed speed) :: [ ServiceStation ] ->
                    speed

                (Speed speed1) :: ServiceStation :: (Speed speed2) :: rest ->
                    if speed2 < speed1 then
                        speed1 + calculateScoreFromRoute (Speed speed2 :: rest)

                    else
                        speed1

                (Speed speed) :: ServiceStation :: ServiceStation :: _ ->
                    speed

                (Speed speed) :: Minus50 :: _ ->
                    speed

                (Speed speed) :: Discard :: _ ->
                    speed

                (Speed speed) :: (DrawCard _) :: _ ->
                    speed

                _ ->
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
        , ( 25, DrawCard 1 )
        , ( 15, DrawCard 2 )
        , ( 25, Discard )
        , ( 4, Speed 10 )
        , ( 4, Speed 20 )
        , ( 5, Speed 30 )
        , ( 5, Speed 40 )
        , ( 6, Speed 50 )
        , ( 6, Speed 60 )
        , ( 7, Speed 70 )
        , ( 6, Speed 80 )
        , ( 6, Speed 90 )
        , ( 5, Speed 100 )
        , ( 5, Speed 110 )
        , ( 4, Speed 120 )
        ]


initGame : StartInfo -> Seed -> GameInfo
initGame startInfo seed =
    let
        numCards =
            10

        createDrawDeck =
            Random.list (numCards * List.length startInfo.players) cardGenerator

        ( drawDeck, newSeed ) =
            Random.step createDrawDeck seed

        takeCards index deck =
            Array.fromList <| List.take numCards <| List.drop (numCards * index) deck

        players =
            Array.fromList <| List.map (\name -> Player name 0 Array.empty Array.empty Nothing Array.empty) startInfo.players
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
