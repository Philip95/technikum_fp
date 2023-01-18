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
            printfn "Insufficient funds"
            false
        | _ -> true

let rec dealerDrawsLoop (dealer : Dealer) : Dealer =
    dealer.Hand
        |> valueHand
        |> function
            | value when value <= DEALER_HIGHEST_SOFT_HIT ->
                let newCard = drawCard()
                printf $" %A{newCard.Rank} of %A{newCard.Suit}\n"
                dealerDrawsLoop { dealer with Hand = dealer.Hand @ [newCard] }
            | _ -> dealer

let dealerPlays (state : State) : State =
    printfn "Dealer's hand:"
    printf $" %A{state.Dealer.Hand[0].Rank} of %A{state.Dealer.Hand[0].Suit}\n"
    let newDealer =  dealerDrawsLoop state.Dealer
    {state with Dealer = newDealer}

let stateAfterPush (state : State) : State =
    { state with
        Player = { state.Player with Balance =  state.Player.Balance + state.Bet; Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

let stateAfterWin (state : State) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + (state.Bet * PAYOUT_FACTOR); Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

let stateAfterLoss (state : State) : State =
    { state with
        Player = { state.Player with Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

let stateAfterBlackjack (state : State) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + (state.Bet * BLACKJACK_PAYOUT_FACTOR); Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

let stateAfterInsurancePayout (state : State) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + (state.Insurance * INSURANCE_PAYOUT_FACTOR); Hand = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

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

    match dealer.Hand |> List.head |> value with
        | value when value = 11 -> printfn "Insurance is possible"
        | _ -> ()

    checkForBlackjack { state with Bet = bet; Player = player; Dealer = dealer }


let checkInsurancePayout (state : State) : Boolean =
    match state.Insurance with
        | insurance when insurance > 0.0 ->
            match valueHand state.Dealer.Hand, state.Dealer.Hand.Length with
                | value, cardCount when value = BLACKJACK && cardCount = 2 ->
                    true
                | _ ->
                    false
        | _ -> false

let endRound (state : State) : State =
    match checkInsurancePayout state with
        | true ->
            printfn "Insurance pays!"
            stateAfterInsurancePayout state
        | false ->
            printfn "Insurance lost!"
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

let checkPlayerHand (state : State) (isEndOfRound : bool) : State =
    match valueHand state.Player.Hand with
        | value when value > BLACKJACK ->
            printfn "Player busts!"
            stateAfterLoss state
        | value when value = BLACKJACK ->
            dealerPlays state |> endRound
        | _ ->
            match isEndOfRound with
                | true -> dealerPlays state |> endRound
                | false -> state

let hit (state : State) : State =
    let player = { state.Player with Hand = state.Player.Hand @ [drawCard()] }

    checkPlayerHand { state with Player = player } false


let stand (state : State) : State =
    dealerPlays state |> endRound

let doubleDown (state : State) : State =
    match verifyBet (state, state.Bet) with
        | false -> state
        | true ->
            let player = { state.Player with Hand = state.Player.Hand @ [drawCard()]; Balance = state.Player.Balance - state.Bet }
            checkPlayerHand { state with Player = player; Bet = state.Bet * 2.0 } true

let split (state : State) : State =
    state


let insurance (state : State) : State =
    match state.Dealer.Hand |> List.head |> value, state.Insurance with
        | dealerHandValue, insurance when dealerHandValue = 11 && insurance = 0.0 ->
            {state with Insurance = state.Bet / 2.0; Player = {state.Player with Balance = state.Player.Balance - (state.Bet / 2.0)}}
        | _, insurance when not (insurance = 0.0) ->
            printfn "Insurance already set"
            state
        | _ ->
            printfn "Insurance not possible. Dealer does not have a ace"
            state

let update (msg : Message) (state : State) : State =
    match msg with
    | Hit -> hit state
    | Stand -> stand state
    | DoubleDown -> doubleDown state
    | Split -> split state
    | Insurance -> insurance state

