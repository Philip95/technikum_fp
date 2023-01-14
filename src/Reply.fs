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

let createStateText (state: State) : string =
    let playerBalance = sprintf "Player balance: %.2f\n" state.Player.Balance
    let currentBet = sprintf "Current bet: %.2f\n" state.Bet

    let playerHandValue = sprintf "Player Hand Value: %i\n" (Game.valueHand state.Player.Hand)
    let playerHand =
        state.Player.Hand
            |> List.map (fun item -> sprintf " %A of %A\n" item.Rank item.Suit)
            |> String.concat ""

    let dealerHandValue = sprintf "Dealer Hand Value: %i\n" (Game.valueHand state.Dealer.Hand)
    let dealerHand =
        state.Dealer.Hand
            |> List.map (fun item -> sprintf " %A of %A\n" item.Rank item.Suit)
            |> String.concat ""

    playerBalance + currentBet + "\n"  + playerHandValue + "Player Hand: \n" + playerHand + "\n" + dealerHandValue + "Dealer Hand: \n" + dealerHand

let evaluate (update : Domain.Message -> State -> State)  (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let message = createStateText newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) = //TODO adapt output
    printfn "%s" outputToPrint
    printf "> "
    state

let roundStart  (state : State) : State =
    printfn "\n---------"
    let message = sprintf "Please place your bet: "
    print (state, message)
    let bet = Console.ReadLine()
    let newState = Game.startRound (float bet) state //TODO check if bet is valid
    print (newState, createStateText newState)
    newState


let rec loop (state : State) : State =
    //TODO game over when player has no funds
     match state.Bet with
        | 0.0 -> roundStart state |> loop
        | _ ->
            Console.ReadLine()
            |> read
            |> evaluate Game.update state
            |> print
            |> loop
