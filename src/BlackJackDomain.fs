namespace BlackJackImpl

module BlackJackDomain =

  type Participant = 
    | Player
    | Dealer

  type GameStatistic = 
    { mutable Won:int 
      mutable Lost:int
      mutable Draw:int }

  type GameCard = 
    { Color : string 
      Name : string
      Value : int 
      isAce : bool}

  type GameResult = 
    | InProcess
    | Won of Participant
    | Draw
  
  type GameState = {
    PlayerCards : GameCard list
    DealerCards : GameCard list 
    CardDeck : GameCard list 
    DealersTurn : bool }

module BlackJackImplementation =
  open BlackJackDomain

  //constants
  let private additionalAceValue = 10
  let private maxValidSum = 21
  let private dealerLimit = 16

  //all Cards in deck
  let private allCards = [
      { Color = "clubs"
        Name = "2" 
        Value = 2
        isAce = false }

      { Color = "clubs"
        Name = "3" 
        Value = 3
        isAce = false}
        
      { Color = "clubs"
        Name = "4" 
        Value = 4
        isAce = false }

      { Color = "clubs"
        Name = "5" 
        Value = 5
        isAce = false}
      
      { Color = "clubs"
        Name = "6" 
        Value = 6
        isAce = false }

      { Color = "clubs"
        Name = "7" 
        Value = 7
        isAce = false}
        
      { Color = "clubs"
        Name = "8" 
        Value = 8
        isAce = false }

      { Color = "clubs"
        Name = "9" 
        Value = 9
        isAce = false}
      
      { Color = "clubs"
        Name = "10" 
        Value = 10
        isAce = false }

      { Color = "clubs"
        Name = "jack" 
        Value = 10
        isAce = false}
        
      { Color = "clubs"
        Name = "queen" 
        Value = 10
        isAce = false }

      { Color = "clubs"
        Name = "king" 
        Value = 10
        isAce = false}

      { Color = "clubs"
        Name = "ace" 
        Value = 1
        isAce = true} 

      { Color = "diamonds"
        Name = "2" 
        Value = 2
        isAce = false }

      { Color = "diamonds"
        Name = "3" 
        Value = 3
        isAce = false}
        
      { Color = "diamonds"
        Name = "4" 
        Value = 4
        isAce = false }

      { Color = "diamonds"
        Name = "5" 
        Value = 5
        isAce = false}
      
      { Color = "diamonds"
        Name = "6" 
        Value = 6
        isAce = false }

      { Color = "diamonds"
        Name = "7" 
        Value = 7
        isAce = false}
        
      { Color = "diamonds"
        Name = "8" 
        Value = 8
        isAce = false }

      { Color = "diamonds"
        Name = "9" 
        Value = 9
        isAce = false}
      
      { Color = "diamonds"
        Name = "10" 
        Value = 10
        isAce = false }

      { Color = "diamonds"
        Name = "jack" 
        Value = 10
        isAce = false}
        
      { Color = "diamonds"
        Name = "queen" 
        Value = 10
        isAce = false }

      { Color = "diamonds"
        Name = "king" 
        Value = 10
        isAce = false}

      { Color = "diamonds"
        Name = "ace" 
        Value = 1
        isAce = true}

      { Color = "hearts"
        Name = "2" 
        Value = 2
        isAce = false }

      { Color = "hearts"
        Name = "3" 
        Value = 3
        isAce = false}
        
      { Color = "hearts"
        Name = "4" 
        Value = 4
        isAce = false }

      { Color = "hearts"
        Name = "5" 
        Value = 5
        isAce = false}
      
      { Color = "hearts"
        Name = "6" 
        Value = 6
        isAce = false }

      { Color = "hearts"
        Name = "7" 
        Value = 7
        isAce = false}
        
      { Color = "hearts"
        Name = "8" 
        Value = 8
        isAce = false }

      { Color = "hearts"
        Name = "9" 
        Value = 9
        isAce = false}
      
      { Color = "hearts"
        Name = "10" 
        Value = 10
        isAce = false }

      { Color = "hearts"
        Name = "jack" 
        Value = 10
        isAce = false}
        
      { Color = "hearts"
        Name = "queen" 
        Value = 10
        isAce = false }

      { Color = "hearts"
        Name = "king" 
        Value = 10
        isAce = false}

      { Color = "hearts"
        Name = "ace" 
        Value = 1
        isAce = true}   

      { Color = "spades"
        Name = "2" 
        Value = 2
        isAce = false }

      { Color = "spades"
        Name = "3" 
        Value = 3
        isAce = false}
        
      { Color = "spades"
        Name = "4" 
        Value = 4
        isAce = false }

      { Color = "spades"
        Name = "5" 
        Value = 5
        isAce = false}
      
      { Color = "spades"
        Name = "6" 
        Value = 6
        isAce = false }

      { Color = "spades"
        Name = "7" 
        Value = 7
        isAce = false}
        
      { Color = "spades"
        Name = "8" 
        Value = 8
        isAce = false }

      { Color = "spades"
        Name = "9" 
        Value = 9
        isAce = false}
      
      { Color = "spades"
        Name = "10" 
        Value = 10
        isAce = false }

      { Color = "spades"
        Name = "jack" 
        Value = 10
        isAce = false}
        
      { Color = "spades"
        Name = "queen" 
        Value = 10
        isAce = false }

      { Color = "spades"
        Name = "king" 
        Value = 10
        isAce = false}

      { Color = "spades"
        Name = "ace" 
        Value = 1
        isAce = true} ]

  let private randomNumberGenerator = new System.Random()
  //helpers
  let private getRandomNumber min max =
    let randomNumber = randomNumberGenerator.Next(min, max+1)
    randomNumber

  //GameLogicHelpers
  let private getPlayerCards gameState = gameState.PlayerCards
  let private getDealerCards gameState = gameState.DealerCards
  let private getCardDeck gameState = gameState.CardDeck
  let private getCardDeckSize gameState = (getCardDeck gameState).Length

  let private containsAces cards = 
    List.exists (fun elem -> elem.isAce) cards

  let getSums cards =
    let mutable sum1 = 0
    let mutable sum2 = 0
    for i in cards do
        sum1 <- sum1 + i.Value
    if containsAces cards then
        sum2 <- sum1 + additionalAceValue
    else 
        sum2 <- sum1
    sum1, sum2

  let getConsideredSum cards = 
    let sum1,sum2 = getSums cards
    if sum2 > maxValidSum then
      sum1
    else 
      max sum1 sum2

  let private isScore21 cards =
    let sum1,sum2 = getSums cards
    sum1 = maxValidSum || sum2 = maxValidSum

  let isBlackJack cards =
    isScore21 cards && cards.Length = 2 

  let private exceeded cards value =
    let consideredSum = getConsideredSum cards
    consideredSum > value

  let private exceeded21 cards = exceeded cards maxValidSum
  let private exceededDealerLimit cards = exceeded cards dealerLimit

  let private playerExceeded21 = exceeded21 << getPlayerCards
  let private dealerExceeded21  = exceeded21 << getDealerCards

  let private playerHasBlackJack = isBlackJack << getPlayerCards
  let private dealerHasBlackJack = isBlackJack << getDealerCards

  let private dealerReachedLimit = exceededDealerLimit << getDealerCards

  let private gameOverByDealer gameState = gameState.DealersTurn && dealerReachedLimit gameState

  let private gameOverByPlayer gameState = not gameState.DealersTurn && (playerExceeded21 gameState || playerHasBlackJack gameState)
  
  let private isGameOver gameState = 
    gameOverByPlayer gameState || gameOverByDealer gameState

    //playerFinished gameState && dealerFinished gameState

  let private getPlayerConsideredSum = getConsideredSum << getPlayerCards
  let private getDealerConsideredSum = getConsideredSum << getDealerCards
  let evaluateGameResult gameState =
    if not (isGameOver gameState) then
      InProcess
    else
      if (playerHasBlackJack gameState) && (dealerHasBlackJack gameState) then
        Draw
      elif playerHasBlackJack gameState then
        Won(Player)
      elif dealerHasBlackJack gameState then
        Won(Dealer)
      elif playerExceeded21 gameState then
        Won(Dealer)
      elif dealerExceeded21 gameState then
        Won(Player)
      elif (getPlayerConsideredSum gameState) = (getDealerConsideredSum gameState) then
        Draw
      else
        if getPlayerConsideredSum gameState > getDealerConsideredSum gameState then
          Won(Player)
        else 
          Won(Dealer)

  let updateStatistic (gameResult : GameResult, stats:GameStatistic) =    
    if gameResult = GameResult.Won(Player) then
        stats.Won <- (stats.Won + 1)
    elif gameResult = GameResult.Won(Dealer) then
        stats.Lost <- (stats.Lost + 1)
    elif gameResult = GameResult.Draw then
        stats.Draw <- (stats.Draw + 1)  
 
  let cardsMatching card1 card2 =
    card1.Color = card2.Color && card1.Name = card2.Name

  let rec private removeCardFromList cards card =
    match cards with
      | [] -> []
      | x::rest -> if cardsMatching x card then
                      (removeCardFromList rest card)
                   else 
                      x::(removeCardFromList rest card)
  
  let private addCardToList (cards:GameCard list) (card:GameCard) = 
    cards @ [card]

  let private givePlayerCard gameState =
    let randomNumber = getRandomNumber 0 (getCardDeckSize gameState)
    let pickedCard = (getCardDeck gameState).Item(randomNumber)

    let newGameState = 
      { PlayerCards = addCardToList (getPlayerCards gameState) pickedCard 
        DealerCards = getDealerCards gameState
        CardDeck = removeCardFromList (getCardDeck gameState) pickedCard
        DealersTurn = gameState.DealersTurn}
    
    newGameState
  
  let private giveDealerToken gameState = 
    let newGameState = 
      { PlayerCards = getPlayerCards gameState
        DealerCards = getDealerCards gameState
        CardDeck = getCardDeck gameState
        DealersTurn = true}
    
    newGameState

  let private giveDealerCard gameState =
    let randomNumber = getRandomNumber 0 (getCardDeckSize gameState)
    let pickedCard = (getCardDeck gameState).Item(randomNumber)

    let newGameState = 
      { PlayerCards = getPlayerCards gameState 
        DealerCards = addCardToList (getDealerCards gameState) pickedCard 
        CardDeck = removeCardFromList (getCardDeck gameState) pickedCard
        DealersTurn = gameState.DealersTurn}
    
    newGameState

  let rec private performStayActions gameState =
    let afterDealerGotCard = giveDealerCard gameState
    if not (isGameOver afterDealerGotCard) then
      performStayActions afterDealerGotCard
    else
      afterDealerGotCard

  let newGame () =

    let gameState = 
      { PlayerCards = []
        DealerCards = [] 
        CardDeck = allCards
        DealersTurn = false }

    let givenPlayerFirstCardState = givePlayerCard gameState
    let givenPlayerSecondCardState = givePlayerCard givenPlayerFirstCardState
    let givenDealerFirstCardState = giveDealerCard givenPlayerSecondCardState
    let givenDealerSecondCardState = giveDealerCard givenDealerFirstCardState

    givenDealerSecondCardState, evaluateGameResult givenDealerSecondCardState

  let hitAction gameState =
    let givenCardToPlayer = givePlayerCard gameState 
    givenCardToPlayer, evaluateGameResult givenCardToPlayer

  let stayAction gameState =
    let gaveDealerToken = giveDealerToken gameState
    if isGameOver gaveDealerToken then
      gaveDealerToken, evaluateGameResult gaveDealerToken
    else
      let afterStayActions = performStayActions gaveDealerToken
      afterStayActions, evaluateGameResult afterStayActions

