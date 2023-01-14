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


let startRound (bet : float) (state : State) : State =
    //TODO check for blackjack at the beginning of the game
    { state with
        Bet = bet;
        Player = { state.Player with Hand = [drawCard(); drawCard()]; Balance = state.Player.Balance - bet};
        Dealer = { state.Dealer with Hand = [drawCard()] }
    }


let endRound (state : State) : State =
    let playerBalance =
        match valueHand state.Player.Hand, valueHand state.Dealer.Hand with
        | playerValue, _ when playerValue > BLACKJACK ->
            printfn "Player busts!"
            printfn "%i" playerValue
            state.Player.Balance - state.Bet
        | _, dealerValue when dealerValue > BLACKJACK ->
            printfn "Dealer busts!"
            state.Player.Balance + (state.Bet * PAYOUT_FACTOR)
        | playerValue, dealerValue when dealerValue = playerValue ->
            printfn "Push!"
            state.Player.Balance
        | playerValue, dealerValue when playerValue > dealerValue ->
            printfn "Player wins!"
            state.Player.Balance + (state.Bet * PAYOUT_FACTOR)
        | _ ->
            printfn "Dealer wins!"
            state.Player.Balance - state.Bet

    { state with
        Player = { state.Player with Balance = playerBalance; Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0 }


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

let bet (bet : float) (state : State) : State =
     {state with
        Bet = bet
        Player = {state.Player with Balance = state.Player.Balance - bet}
    }

let hit (state : State) : State =
    let player = { state.Player with Hand = state.Player.Hand @ [drawCard()] }
    let newState = { state with Player = player }

    match valueHand player.Hand with //TODO maybe adapt this?
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

