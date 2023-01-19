module Game

open System
open Domain

///returns the value of a hand
let valueHand (hand:Hand) : int =
    let initialValue = hand |> List.map value |> List.sum
    let numAces = hand |> List.filter (fun card -> card.Rank = Ace) |> List.length
    match initialValue with
        | value when value > 21 -> initialValue - (numAces * 10)
        | value -> value

///returns a new Card
let drawCard () : Card =
    let suits = [Clubs; Diamonds; Hearts; Spades]
    let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]
    let random = System.Random()
    let suitIndex = random.Next(List.length suits)
    let rankIndex = random.Next(List.length ranks)
    { Suit = List.item suitIndex suits; Rank = List.item rankIndex ranks }

///checks if the bet is valid
let isBetValid (state: State, bet : float) : bool =
    match bet with
        | bet when bet <= 0.0 -> //bet is less than or equal to 0
            printfn "Bet must be greater than 0\n"
            false
        | bet when bet > state.Player.Balance -> //bet is greater than the players balance
            printfn "Insufficient funds\n"
            false
        | _ -> true

///draws a card to the current hand
let drawCardToHand (index: int) (hands: Hand list) =
    List.mapi (fun i hand -> if i = index then drawCard() :: hand else hand) hands

///returns a new state at the end of a round
let resetState (state: State, payout: float) : State =
    { state with
        Player = { state.Player with Balance = state.Player.Balance + payout; Hands = [] };
        Dealer = { state.Dealer with Hand = [] };
        Bet = 0.0
        Insurance = 0.0}

///dealer draws until they reach 17 or higher
let rec dealerDrawsLoop (dealer : Dealer) : Dealer =
    dealer.Hand
        |> valueHand
        |> function
            | value when value <= DEALER_HIGHEST_SOFT_HIT ->
                let newCard = drawCard()
                printf $" %A{newCard.Rank} of %A{newCard.Suit}\n"
                dealerDrawsLoop { dealer with Hand = dealer.Hand @ [newCard] }
            | _ -> dealer

///dealers turn to draw cards
let dealerPlays (state : State) : State =
    printfn "Dealer's hand:"
    printfn $" %A{state.Dealer.Hand[0].Rank} of %A{state.Dealer.Hand[0].Suit}"
    let newDealer =  dealerDrawsLoop state.Dealer
    printfn $"Dealer Hand Value: %i{valueHand newDealer.Hand}\n";
    {state with Dealer = newDealer}

///checks if the player has blackjack
let checkForBlackjack (state : State) : State =
    match valueHand state.Player.Hands[state.CurrentHand], valueHand state.Dealer.Hand with
        | valuePlayer, valueDealer when valuePlayer = BLACKJACK && valueDealer = BLACKJACK ->
            printfn "Push!\n"
            resetState (state, payout state.Bet Push)
        | valuePlayer, _ when valuePlayer = BLACKJACK ->
            printfn "Blackjack!\n"
            resetState (state, payout state.Bet Blackjack)
        | _ -> state

///starts a new round
let startRound (bet : float) (state : State) : State =
    let playerHand : Hand = [drawCard(); drawCard()]
    let player = { state.Player with Hands = [playerHand]; Balance = state.Player.Balance - bet }
    let dealer = { state.Dealer with Hand = [drawCard()] }

    //output a message when insurance is possible
    match dealer.Hand |> List.head |> value with
        | value when value = 11 -> printfn "Insurance is possible\n"
        | _ -> ()

    checkForBlackjack { state with Bet = bet; Player = player; Dealer = dealer }

///checks if insurance is payed out when set
let isInsurancePayout (state : State) : Boolean =
    match state.Insurance, valueHand state.Dealer.Hand, state.Dealer.Hand.Length with
        | insurance, value, cardCount when insurance > 0.0 && value = BLACKJACK && cardCount = 2->
            printfn "Insurance pays!\n"
            true
        | insurance, _, _ when insurance > 0.0 ->
            printfn "Insurance lost!\n"
            false
        | _ -> false

///calculates the payout for a round
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
            | playerValue, _ when playerValue > BLACKJACK -> //player busts
                printfn "Player busts!\n"
                payout bet Loose + getPayout (tl, dealerHand, bet)
            | _, dealerValue when dealerValue > BLACKJACK -> //dealer busts
                printfn "Dealer busts!\n"
                payout bet Win + getPayout (tl, dealerHand, bet)
            | playerValue, dealerValue when dealerValue = playerValue -> //push
                printfn "Push!\n"
                payout bet Push + getPayout (tl, dealerHand, bet)
            | playerValue, dealerValue when playerValue > dealerValue -> //player wins
                printfn "Player wins!\n"
                payout bet Win + getPayout (tl, dealerHand, bet)
            | _ -> //dealer wins
                printfn "Dealer wins!\n"
                payout bet Loose + getPayout (tl, dealerHand, bet)


///ends the round
let endRound (state : State) : State =
    match isInsurancePayout state with
        | true ->
            let payout = payout state.Insurance Insurance
            resetState (state, payout)
        | false ->
            let payout = getPayout (state.Player.Hands, state.Dealer.Hand, state.Bet)
            resetState (state, payout)

///checks if the player hand is over
let isPlayerHandEnded (hand : Hand) : Boolean =
    match valueHand hand with
        | value when value > BLACKJACK -> //bust
            true
        | value when value = BLACKJACK -> //blackjack
            true
        | value ->
            false

///checks if the player has more hands to play
let checkLastHand (state: State) : State =
    match state.Player.Hands.Length with
        | length when length = state.CurrentHand + 1 -> dealerPlays state |> endRound //last hand
        | _ ->  { state with CurrentHand = state.CurrentHand + 1 }

///player hits - draws a card to the current hand
let hit (state : State) : State =
    let player = { state.Player with Hands = drawCardToHand state.CurrentHand state.Player.Hands }
    let newState = { state with Player = player }

    match isPlayerHandEnded newState.Player.Hands[state.CurrentHand] with
        | true ->
            printfn "Player hand ended!\n"
            checkLastHand newState
        | false ->
            newState

///player stands - ends the current hand
let stand (state : State) : State =
    checkLastHand state

///player doubles down - doubles the bet and draws a card
let doubleDown (state : State) : State =
    let isBetValid = isBetValid (state, state.Bet)
    match state.Player.Hands.Length with
        | cardCount when cardCount = 1 && isBetValid ->
            let playerHand = state.Player.Hands.[state.CurrentHand]
            //player draws onw more card
            let player = { state.Player with Hands = [drawCard() :: playerHand]; Balance = state.Player.Balance - state.Bet }
            let newState = { state with Player = player; Bet = state.Bet * 2.0 }

            //round ends
            dealerPlays newState |> endRound
        | _ when isBetValid = false -> state //bet is not valid
        | _ -> //double down is not allowed when splitting
            printfn "Double down is not allowed when splitting\n"
            state


///player splits - splits the current hand into two hands
let split (state : State) : State =
    let isBetValid = isBetValid (state, state.Bet)
    match state.Player.Hands[state.CurrentHand], state.Player.Hands[state.CurrentHand].Length with
        | hand, length when hand[0].Rank = hand[1].Rank && length = 2 && isBetValid ->
            let playerHand = state.Player.Hands.[state.CurrentHand]
            let updatedPlayerHand: Hand list = playerHand |> List.map (fun card -> [card; drawCard()])
            let player = { state.Player with Hands = updatedPlayerHand; Balance = state.Player.Balance - state.Bet }
            let newState = { state with Player = player}

            match value playerHand[0], value playerHand[1] with
                | card0, card1 when card0 = 11 && card1 = 11 -> //player has two aces - game ends
                    dealerPlays newState |> endRound
                | _ ->
                    //check if the player hand is over
                    match isPlayerHandEnded updatedPlayerHand[0], isPlayerHandEnded updatedPlayerHand[1] with
                        | true, true -> dealerPlays newState |> endRound
                        | _, _ -> newState
        | _ when isBetValid = false -> state //bet is not valid
        | _ -> //split is not allowed when splitting
            printfn "Split is not allowed when the hand is not a pair\n"
            state

///player takes insurance - takes half the bet and pays 2:1 if the dealer has a blackjack
let insurance (state : State) : State =
    match state.Dealer.Hand |> List.head |> value, state.Insurance with
        | dealerHandValue, insurance when dealerHandValue = 11 && insurance = 0.0 ->
            {state with Insurance = state.Bet / 2.0; Player = {state.Player with Balance = state.Player.Balance - (state.Bet / 2.0)}}
        | _, insurance when not (insurance = 0.0) -> //insurance already set
            printfn "Insurance already set\n"
            state
        | _ -> //insurance not possible - dealer does not have a ace
            printfn "Insurance not possible. Dealer does not have a ace\n"
            state

///updates the state based on the message
let update (msg : Message) (state : State) : State =
    match msg with
    | Hit -> hit state
    | Stand -> stand state
    | DoubleDown -> doubleDown state
    | Split -> split state
    | Message.Insurance -> insurance state
