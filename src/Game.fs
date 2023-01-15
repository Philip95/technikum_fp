module Game

open System
open Domain

let valueHand (hand:Hand) : int =
    let initialValue = hand |> List.map value |> List.sum
    let numAces = hand |> List.filter (fun card -> card.Rank = Ace) |> List.length
    match initialValue with
        | value when value > 21 -> initialValue - (numAces * 10)
        | value -> value


let drawCard () : Card =
    let suits = [Clubs; Diamonds; Hearts; Spades]
    let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]
    let random = System.Random()
    let suitIndex = random.Next(List.length suits)
    let rankIndex = random.Next(List.length ranks)
    { Suit = List.item suitIndex suits; Rank = List.item rankIndex ranks }

let verifyBet (state: State, bet : float) : bool =
    match bet with
        | bet when bet <= 0.0 ->
            printfn "Bet must be greater than 0"
            false
        | bet when bet > state.Player.Balance ->
            printfn "Bet must be less than your balance"
            false
        | _ -> true

let rec dealerDrawsLoop (dealer : Dealer) : Dealer =
    dealer.Hand
        |> valueHand
        |> function
            | value when value <= DEALER_HIGHEST_SOFT_HIT ->
                let newCard = drawCard()
                printf " %A of %A\n" newCard.Rank newCard.Suit
                dealerDrawsLoop { dealer with Hand = dealer.Hand @ [newCard] }
            | _ -> dealer

let dealerPlays (state : State) : State =
    printfn "Dealer's hand:"
    printf " %A of %A\n" state.Dealer.Hand[0].Rank state.Dealer.Hand[0].Suit
    let newDealer =  dealerDrawsLoop state.Dealer
    {state with Dealer = newDealer}

let stateAfterPush (state : State) : State =
    { state with
        Player = { state.Player with Balance =  state.Player.Balance + state.Bet; Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0 }

let stateAfterWin (state : State) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + (state.Bet * PAYOUT_FACTOR); Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0 }

let stateAfterLoss (state : State) : State =
    { state with
        Player = { state.Player with Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0 }

let stateAfterBlackjack (state : State) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + (state.Bet * BLACKJACK_PAYOUT_FACTOR); Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0 }

let checkForBlackjack (state : State) : State =
    match valueHand state.Player.Hand with
        | value when value = BLACKJACK ->
            match valueHand state.Dealer.Hand with
                | value when value = BLACKJACK ->
                    printfn "Push!"
                    stateAfterPush state
                | _ ->
                    printfn "Blackjack!"
                    stateAfterBlackjack state
        | _ -> state

let startRound (bet : float) (state : State) : State =
    let player = { state.Player with Hand = [drawCard(); drawCard()]; Balance = state.Player.Balance - bet }
    let dealer = { state.Dealer with Hand = [drawCard()] }

    checkForBlackjack { state with Bet = bet; Player = player; Dealer = dealer }


let endRound (state : State) : State =
    match valueHand state.Player.Hand, valueHand state.Dealer.Hand with
        | playerValue, _ when playerValue > BLACKJACK ->
            printfn "Player busts!"
            stateAfterLoss state
        | _, dealerValue when dealerValue > BLACKJACK ->
            printfn "Dealer busts!"
            stateAfterWin state
        | playerValue, dealerValue when dealerValue = playerValue ->
            printfn "Push!"
            stateAfterPush state
        | playerValue, dealerValue when playerValue > dealerValue ->
            printfn "Player wins!"
            stateAfterWin state
        | _ ->
            printfn "Dealer wins!"
            stateAfterLoss state


let hit (state : State) : State =
    let player = { state.Player with Hand = state.Player.Hand @ [drawCard()] }
    let newState = { state with Player = player }

    match valueHand player.Hand with
        | value when value > BLACKJACK ->
            endRound newState
        | value when value = BLACKJACK ->
            dealerPlays newState |> endRound
        | _ -> newState


let stand (state : State) : State =
    dealerPlays state |> endRound

let doubleDown (state : State) : State =
    //TODO double down
    state

let split (state : State) : State =
    //TODO split
    state

let insurance (state : State) : State =
    //TODO insurance
    state

let update (msg : Message) (state : State) : State =
    match msg with
    | Hit -> hit state
    | Stand -> stand state
    | DoubleDown -> doubleDown state
    | Split -> split state
    | Insurance -> insurance state

