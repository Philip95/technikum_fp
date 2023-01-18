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
            printfn "Insufficient funds\n"
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
    printfn $" %A{state.Dealer.Hand[0].Rank} of %A{state.Dealer.Hand[0].Suit}"
    let newDealer =  dealerDrawsLoop state.Dealer
    printfn $"Dealer Hand Value: %i{valueHand newDealer.Hand}\n";
    {state with Dealer = newDealer}

let resetState (state: State, payout: float) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + payout; Hands = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

let checkForBlackjack (state : State) : State =
    match valueHand state.Player.Hands[state.currentHand] with
        | value when value = BLACKJACK ->
            match valueHand state.Dealer.Hand with
                | value when value = BLACKJACK ->
                    let payout = payout state.Bet Push
                    resetState (state, payout)
                | _ ->
                    printfn "Blackjack!"
                    let payout = payout state.Bet Blackjack
                    resetState (state, payout)
        | _ -> state

let startRound (bet : float) (state : State) : State =
    let playerHand : Hand = [drawCard(); drawCard()]
    let player = { state.Player with Hands = [playerHand]; Balance = state.Player.Balance - bet }
    let dealer = { state.Dealer with Hand = [drawCard()] }

    match dealer.Hand |> List.head |> value with
        | value when value = 11 -> printfn "Insurance is possible\n"
        | _ -> ()

    checkForBlackjack { state with Bet = bet; Player = player; Dealer = dealer }


let isInsurancePayout (state : State) : Boolean =
    match state.Insurance with
        | insurance when insurance > 0.0 ->
            match valueHand state.Dealer.Hand, state.Dealer.Hand.Length with
                | value, cardCount when value = BLACKJACK && cardCount = 2 ->
                    printfn "Insurance pays!"
                    true
                | _ ->
                    printfn "Insurance lost!"
                    false
        | _ -> false


let addToPayout (currentPayout: float, value: float) : float =
    currentPayout + value

let rec getPayout (playerHandList: Hand list, dealerHand: Hand, bet: float) : float =
    match playerHandList with
    | [] -> 0.0
    | hd::tl ->
        printfn $"Hand Value: %i{valueHand hd}";
        hd
        |> List.map (fun item -> $" %A{item.Rank} of %A{item.Suit}\n")
        |> String.concat ""
        |> printfn "%s"

        match valueHand hd, valueHand dealerHand with
            | playerValue, _ when playerValue > BLACKJACK ->
                printfn "Player busts!\n"
                payout bet Loose + getPayout (tl, dealerHand, bet)
            | _, dealerValue when dealerValue > BLACKJACK ->
                printfn "Dealer busts!\n"
                payout bet Win + getPayout (tl, dealerHand, bet)
            | playerValue, dealerValue when dealerValue = playerValue ->
                printfn "Push!\n"
                payout bet Push + getPayout (tl, dealerHand, bet)
            | playerValue, dealerValue when playerValue > dealerValue ->
                printfn "Player wins!\n"
                payout bet Win + getPayout (tl, dealerHand, bet)
            | _ ->
                printfn "Dealer wins!\n"
                payout bet Loose + getPayout (tl, dealerHand, bet)



let endRound (state : State) : State =
    match isInsurancePayout state with
        | true ->
            let payout = payout state.Insurance Insurance
            resetState (state, payout)
        | false ->
            let payout = getPayout (state.Player.Hands, state.Dealer.Hand, state.Bet)
            resetState (state, payout)

let isPlayerHandEnded (hand : Hand) : Boolean =
    match valueHand hand with
        | value when value > BLACKJACK ->
            true
        | value when value = BLACKJACK ->
            true
        | value ->
            false

let checkLastHand (state: State) : State =
    match state.Player.Hands.Length with
        | length when length = state.currentHand + 1 -> dealerPlays state |> endRound
        | _ ->  { state with currentHand = state.currentHand + 1 }

let drawCardToHand (index: int) (hands: Hand list) =
    List.mapi (fun i hand -> if i = index then drawCard() :: hand else hand) hands

let hit (state : State) : State =
    let player = { state.Player with Hands = drawCardToHand state.currentHand state.Player.Hands }
    let newState = { state with Player = player }

    match isPlayerHandEnded newState.Player.Hands[state.currentHand] with
        | true ->
            printfn "Player hand ended!"
            checkLastHand newState
        | false ->
            printfn "hand not ended"
            newState


let stand (state : State) : State =
    checkLastHand state

let doubleDown (state : State) : State =
    match state.Player.Hands.Length with
        | 1 ->
            match verifyBet (state, state.Bet) with
                | false -> state
                | true ->
                    let playerHand = state.Player.Hands.[state.currentHand]
                    let player = { state.Player with Hands = [drawCard() :: playerHand]; Balance = state.Player.Balance - state.Bet }
                    let newState = { state with Player = player; Bet = state.Bet * 2.0 }
                    match isPlayerHandEnded newState.Player.Hands[state.currentHand] with
                        | true -> dealerPlays newState |> endRound
                        | false -> state
        | _ ->
            printfn "Double down is not allowed when splitting"
            state


let splitSublistsAt (lists: list<list<int>>) (index: int) =
    let sublist = lists.[index]
    sublist |> List.map (fun x -> [x])

let split (state : State) : State =
    match state.Player.Hands[state.currentHand], state.Player.Hands[state.currentHand].Length with
        | hand, length when hand.[0].Rank = hand.[1].Rank && length = 2 ->
            match verifyBet (state, state.Bet) with
                | false -> state
                | true ->
                    let playerHand = state.Player.Hands.[state.currentHand]
                    let updatedPlayerHand: Hand list = playerHand |> List.map (fun card -> [card; drawCard()])
                    let player = { state.Player with Hands = updatedPlayerHand; Balance = state.Player.Balance - state.Bet }

                    match playerHand[0], playerHand[1] with
                        | card0, card1 when value card0 = 11 && value card1 = 11 ->
                            dealerPlays state |> endRound
                        | _ ->
                            let newState = { state with Player = player}
                            match isPlayerHandEnded updatedPlayerHand[0] with
                                | true ->
                                    match isPlayerHandEnded updatedPlayerHand[1] with
                                        | true -> checkLastHand newState
                                        | false -> newState
                                | false -> newState
        | _, _ ->
            printfn "Split is not possible"
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
    | Message.Insurance -> insurance state
