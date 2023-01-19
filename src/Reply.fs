module Reply

open System
open Microsoft.FSharp.Reflection

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State

let read (input : string) =
    match input.ToUpper() with
    | "HIT" -> Domain.Hit |> DomainMessage
    | "STAND" -> Domain.Stand |> DomainMessage
    | "DOUBLEDOWN" -> Domain.DoubleDown |> DomainMessage
    | "SPLIT" -> Domain.Split |> DomainMessage
    | "INSURANCE" -> Domain.Insurance |> DomainMessage
    | "HELP" -> HelpRequested
    | _  -> NotParsable input

///returns a string with all known commands
let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

///returns a string with the rank and suit of each card in the hand
let createStateTextHand (hand : Domain.Hand) : string =
    hand
    |> List.map (fun item -> $" %A{item.Rank} of %A{item.Suit}\n")
    |> String.concat ""

///returns a string with a hand and its value
/// loops through all hands in the player's hand list
let rec createStateTextFromHandLoop (handList: Domain.Hand list, currentHand: int, index : int) : string =
    match handList with
    | [] -> ""
    | hd::tl ->
        //when the hand is the current played add [current hand] to the string
        let currentHandString =
            match currentHand with
                | currentHand when currentHand = index -> $"[current hand]";
                | _ -> ""

        let messages = [
            currentHandString;
            $"Hand Value: %i{Game.valueHand hd}";
            createStateTextHand hd
        ]

        let message = String.concat "\n" messages
        message + createStateTextFromHandLoop (tl, currentHand, index+1)

///returns a string with the current state of the game
let createStateText (state: State) : string =
    match state.Bet with
    | 0.00 -> $"Player balance: %.2f{state.Player.Balance}";
    | _ ->
        let messages = [
            $"Player balance: %.2f{state.Player.Balance}";
            $"Current bet: %.2f{state.Bet}";
            $"Current Insurance Value: %.2f{state.Insurance}\n";
            $"Player Hands:\n";
            createStateTextFromHandLoop (state.Player.Hands, state.CurrentHand, 0);
            $"Dealer Hand Value: %i{Game.valueHand state.Dealer.Hand}";
            createStateTextHand state.Dealer.Hand
        ]

        String.concat "\n" messages

///returns a updated state and a string with the output
let evaluate (update: Domain.Message -> State -> State)  (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        (newState, createStateText newState)
    | HelpRequested ->
        (state, createHelpText ())
    | NotParsable originalInput ->
        let message =
            $""""%s{originalInput}" was not parsable. {"You can get information about known commands by typing \"Help\""}"""
        (state, message)

///prints the output to the console
let print (state : State, outputToPrint : string) =
    printfn $"%s{outputToPrint}"
    printf "> "
    state

///tries to place a bet, if the input is not a number or the bet is invalid it will ask again
let rec tryPlaceBet (state : State) : float =
    let message = "Please place your bet: "
    print (state, message) |> ignore
    let bet = Console.ReadLine()
    try
        let bet = float bet
        match Game.isBetValid (state, bet) with
            | false -> tryPlaceBet state
            | true -> bet
    with
        | :? FormatException ->
            printfn "Invalid input, please enter a number\n"
            tryPlaceBet state

///starts a new round
let roundStart (state : State) : State =
    printfn "\n---------"
    let bet = tryPlaceBet state
    let newState = Game.startRound (float bet) state
    print (newState, createStateText newState)

///loops the game
let rec loop (state : State) : State =
     match state.Bet, state.Player.Balance with
        //if the player has no funds left the game is over
        | bet, balance when bet = 0.0 && balance = 0.0 ->
            print (state, "\nGame over, you have no funds left\n")
        //start a new round if the player has no bet
        | bet, _ when bet = 0.0 -> roundStart state |> loop
        | _ ->
            Console.ReadLine()
            |> read
            |> evaluate Game.update state
            |> print
            |> loop
