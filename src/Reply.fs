module Reply

open System
open Microsoft.FSharp.Reflection
open Parser

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

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let createStateTextHand (hand : Domain.Hand) : string =
    hand
    |> List.map (fun item -> $" %A{item.Rank} of %A{item.Suit}\n")
    |> String.concat ""

let rec createStateTextFromHandLoop (handList: Domain.Hand list, currentHand: int, index : int) : string =
    match handList with
    | [] -> ""
    | hd::tl ->
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


let createStateText (state: State) : string =

    match state.Bet with
    | 0.00 -> $"Player balance: %.2f{state.Player.Balance}";
    | _ ->
        let messages = [
            $"Player balance: %.2f{state.Player.Balance}";
            $"Current bet: %.2f{state.Bet}";
            $"Current Insurance Value: %.2f{state.Insurance}\n";
            $"Player Hands:\n";
            createStateTextFromHandLoop (state.Player.Hands, state.currentHand, 0);
            $"Dealer Hand Value: %i{Game.valueHand state.Dealer.Hand}";
            createStateTextHand state.Dealer.Hand
        ]

        String.concat "\n" messages

let evaluate (update : Domain.Message -> State -> State)  (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = Game.update msg state
        (newState, createStateText newState)
    | HelpRequested ->
        (state, createHelpText ())
    | NotParsable originalInput ->
        let message =
            $""""%s{originalInput}" was not parsable. {"You can get information about known commands by typing \"Help\""}"""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn $"%s{outputToPrint}"
    printf "> "
    state

let rec tryPlaceBet (state : State) : float =
    let message = sprintf "Please place your bet: "
    print (state, message) |> ignore
    let bet = Console.ReadLine()
    try
        let bet = float bet
        match Game.verifyBet (state, bet) with
            | false -> tryPlaceBet state
            | true -> bet
    with
        | :? System.FormatException ->
            printfn "Invalid input, please enter a number"
            tryPlaceBet state

let roundStart (state : State) : State =
    printfn "\n---------"
    let bet = tryPlaceBet state
    let newState = Game.startRound (float bet) state
    print (newState, createStateText newState)

let rec loop (state : State) : State =
     match state.Bet, state.Player.Balance with
        | bet, balance when bet = 0.0 && balance = 0.0 ->
            print (state, "Game over, you have no funds left")
        | bet, _ when bet = 0.0 -> roundStart state |> loop
        | _ ->
            Console.ReadLine()
            |> read
            |> evaluate Game.update state
            |> print
            |> loop
