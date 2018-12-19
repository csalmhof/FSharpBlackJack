module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

open BlackJackImpl.BlackJackDomain
open BlackJackImpl.BlackJackImplementation

type GameInfo = 
    { Id:int
      State: GameState
      Result: GameResult }

type Game =
    | GameInProcess of GameInfo
    | GameFinished of GameInfo

type Model =
    { GameNumber : int
      Games : Game list
      GameStats : GameStatistic }

type Msg =
| CreateGame
| HitCard of GameInfo
| PlayerStays of GameInfo

let init() : Model =
    { GameNumber = 1
      Games = []
      GameStats = { Won = 0 
                    Lost = 0 
                    Draw = 0 }}

// UPDATE

let hit (gameInfo : GameInfo, model : Model) (d : Game) =
    match d with
    | GameInProcess t ->
        if t.Id = gameInfo.Id then
            let newGameState,newGameResult = hitAction gameInfo.State
            let newGameInfo = {Id=gameInfo.Id;State=newGameState;Result=newGameResult}

            sprintf "Player got Card for Game Nr. %d" t.Id
            |> Browser.console.log
            if not (newGameInfo.Result = InProcess) then                       
                updateStatistic(newGameResult ,model.GameStats)
                (GameFinished(newGameInfo))
            else
                (GameInProcess(newGameInfo))
        else d
    | GameFinished t ->
        if t.Id = gameInfo.Id then
            sprintf "No further action for Game Nr. %d allowed" t.Id
            |> Browser.console.log
            (GameFinished (t))
        else d

let stay (gameInfo : GameInfo, model : Model) (d : Game) =
    match d with
    | GameInProcess t ->
        if t.Id = gameInfo.Id then 
            let newGameState,newGameResult = stayAction gameInfo.State
            let newGameInfo = {Id=gameInfo.Id;State=newGameState;Result=newGameResult}

            updateStatistic(newGameResult ,model.GameStats)

            sprintf "Dealer got Cards for Game Nr. %d" t.Id
            |> Browser.console.log
                
            (GameFinished newGameInfo) 
        else d
    | GameFinished _ -> d

let update (msg:Msg) (model:Model) =
    match msg with
    | CreateGame ->
        let newGameState,newGameResult = newGame ()
        let newDraft = GameInProcess {Id=model.GameNumber;State=newGameState;Result=newGameResult};
        { model with
            GameNumber = model.GameNumber + 1
            Games = newDraft::model.Games }
    | HitCard gameInfo ->
        let games =
            model.Games
            |> List.map (hit (gameInfo, model))
        { model with Games = games }
    | PlayerStays gameInfo ->
        let games = 
            model.Games
            |> List.map (stay (gameInfo, model))
        { model with Games = games }

// VIEW (rendered with React)

open Fulma
let getCardImg card = 
    "img/" + card.Color + "-" + card.Name + ".png"

let getFirstCard cardList = 
    cardList 
    |> Seq.map(fun r -> getCardImg r) 
    |> Seq.map(fun r -> img [ Class "content-card"
                              Src r])
    |> Seq.item 0

let getHigherSum cardList =
    getConsideredSum cardList

let intToStr num =
    sprintf "%d" num 

let playerInfoString playerCards =
    let sum1,sum2 = getSums playerCards
    sprintf "Player has (%d/%d)" sum1 sum2

let dealerInfoString dealerCards =
    let sum1,sum2 = getSums dealerCards
    sprintf "Dealer has (%d/%d)" sum1 sum2

let gameInfoString dealerCards playerCards =
    sprintf "%s, %s" (playerInfoString playerCards) (dealerInfoString dealerCards)

let parseResult gameResult =
    match gameResult with
        | InProcess -> "InProcess"
        | Won(Player) -> "Won By Player"
        | Won(Dealer) -> "Won By Dealer"
        | Draw -> "Draw"

let gameCardTileStyle =
    Style [
        Padding 15
        TextAlign "center"
        VerticalAlign "middle"
        BackgroundColor "darkgreen"
        BoxShadow "0 0 3px black"
        BorderStyle "inset"
        BorderColor "SaddleBrown "
        MarginBottom -5
    ]

let playerStateStyle =
    Style [
        Padding 15
        TextAlign "center"
        Margin 5
        VerticalAlign "middle"
    ]

let openBlackJackGameTile dispatch (info : GameInfo) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str (sprintf "PLAYING: Game Nr.: %d" info.Id) ] ]
              Card.content []
                [ Content.content [] [
                    div [playerStateStyle] [ str (playerInfoString info.State.PlayerCards) ]
                    div [gameCardTileStyle] [
                        for n in info.State.PlayerCards ->
                            img  [ Class "content-card"
                                   Src (getCardImg n) ] ]
                    div [gameCardTileStyle] [
                        (getFirstCard info.State.DealerCards)
                        img [ Class "content-card" 
                              Src "img/folded.png" ] ] ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> HitCard info |> dispatch) ] ]
                    [ str "Hit" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> PlayerStays info |> dispatch) ] ]
                    [ str "Stay" ] ] ] ]
                    
let finishedBlackJackGameTile dispatch (info : GameInfo) =
       Tile.tile [ Tile.IsChild; Tile.Size Tile.Is12; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str (sprintf "FINISHED: Game Nr.: %d, GameStatus: %s" info.Id (parseResult info.Result)) ] ]
              Card.content []
                [ Content.content [] [ 
                    div [playerStateStyle] [ str (gameInfoString info.State.DealerCards info.State.PlayerCards) ]
                    div [gameCardTileStyle] [
                        for n in info.State.PlayerCards ->
                            img  [ Class "content-card"
                                   Src (getCardImg n) ] ]  
                    div [gameCardTileStyle] [
                        for n in info.State.DealerCards ->
                            img  [ Class "content-card"
                                   Src (getCardImg n) ] ] ] ]
              Card.footer []
                [ ] ] ]

let toCard dispatch (actualGame : Game) =
    match actualGame with
    | GameInProcess game ->
        openBlackJackGameTile dispatch game
    | GameFinished game ->
        finishedBlackJackGameTile dispatch game

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let rec chunkByOne soFar l =
    match l with
    | [x1] ->
        [x1]::soFar
    | x1::xs ->
        chunkByOne ([x1]::soFar) xs
    | xs ->
        xs::soFar

let toCardRows dispatch (titles : Game list) =
    titles
    |> chunkByOne []
    |> List.rev
    |> List.map ((List.map (toCard dispatch)) >> toCardRow)


let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "FSharp BlackJack" ] ] ]
        Container.container [ Container.IsFluid ]
          [ h1 [ Class "is-size-1 app-title" ] [ str "Play a Game of BlackJack" ]
            Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                [ yield Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12]
                    [ Tile.tile [ Tile.IsChild ]
                        [ Card.card []
                            [ Card.content []
                                [ Content.content [] 
                                    [ str (sprintf "Won: %d / Lost: %d / Draw: %d" model.GameStats.Won model.GameStats.Lost model.GameStats.Draw) ] ]
                              Card.footer []
                                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateGame) ] ]
                                    [ str "Start a new Game" ] ] ] ] ]
                  yield! model.Games |> toCardRows dispatch ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
