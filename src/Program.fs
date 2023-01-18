[<EntryPoint>]
let main argv =
    let initialState = Domain.init ()
    printfn "Welcome to the Blackjack table!\n"
    printfn "Blackjack pays 3 to 2"
    printfn "Dealer must draw to 16 and stand on all 17's"
    printfn "Insurance pays 2 to 1"
    printfn "You start out with a balance of %.0f" initialState.Player.Balance

    Reply.loop initialState |> ignore
    0
