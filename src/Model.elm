module Model exposing (..)

import Array exposing (Array)
import Bool.Extra
import Maybe.Extra
import Random exposing (Seed)
import Time exposing (Posix)


type alias Model =
    { gameState : GameState }


type GameState
    = Start StartInfo
    | Play GameInfo
    | End (List ( String, Int ))


type alias StartInfo =
    { players : List ( String, Bool ) }


type alias GameInfo =
    { players : Array Player
    , stageNumber : Int
    , roundNumber : Int
    , roundState : RoundState
    , sharedPile : Array Card
    , sharedPileCard : Maybe Card
    , sharedPileSum : Int
    , randomnessSeed : Seed
    }


type RoundState
    = NextPlayerInTurn Int
    | PlayerInTurn Int
    | RevealSharedPileCard
    | RevealSharedPileCardNextPlayerInTurn Int
    | RevealSharedPileCardPlayerInTurn Int
    | StageEnd


type alias Player =
    { name : String
    , score : Int
    , hand : Array Card
    , route : Array Card
    , selectedHandCardIndex : Maybe Int
    , cardsToRoute : Array Card
    , ai : Maybe Seed
    }


type Card
    = Speed Int
    | Minus50
    | ServiceStation
    | DrawCard Int
    | Discard


type Msg
    = NameChanged Int String
    | SetPlayerAI Int Bool
    | RemovePlayer Int
    | AddPlayer
    | StartGame StartInfo
    | GameStarted StartInfo Seed
    | StartTurnClicked
    | HandCardClicked Int Int
    | AddToRouteClicked Int Int
    | TakeRouteCardBackClicked Int Card
    | AddToSharedPileClicked Int
    | TakeSharedPileCardBackClicked Int Card
    | EndTurnClicked
    | RevealSharedPileCardClicked
    | NextStageClicked
    | EndGameClicked
    | Tick Posix


updateStartInfo : (StartInfo -> StartInfo) -> Model -> Model
updateStartInfo updateFunction model =
    case model.gameState of
        Start startInfo ->
            { model | gameState = Start (updateFunction startInfo) }

        _ ->
            model


updateGameInfo : (GameInfo -> GameInfo) -> Model -> Model
updateGameInfo updateFunction model =
    case model.gameState of
        Play gameInfo ->
            { model | gameState = Play (updateFunction gameInfo) }

        _ ->
            model


updatePlayerInModel : Int -> (Player -> Player) -> Model -> Model
updatePlayerInModel playerIndex updateFunction model =
    updateGameInfo (\gameInfo -> { gameInfo | players = Array.indexedMap (\index player -> Bool.Extra.ifElse (updateFunction player) player (index == playerIndex)) gameInfo.players }) model


updatePlayerInPlayers : Int -> (Player -> Player) -> Array Player -> Array Player
updatePlayerInPlayers playerIndex updateFunction players =
    Array.indexedMap (\index player -> Bool.Extra.ifElse (updateFunction player) player (index == playerIndex)) players


getSelectedCardFromPlayersHand : Int -> Array Player -> Maybe Card
getSelectedCardFromPlayersHand playerIndex players =
    let
        currentPlayer =
            Array.get playerIndex players
    in
    Maybe.Extra.join <| Maybe.map (\player -> Maybe.Extra.unwrap Nothing (\selectedHandCardIndex -> Array.get selectedHandCardIndex player.hand) player.selectedHandCardIndex) currentPlayer
