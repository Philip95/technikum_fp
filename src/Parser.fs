module Parser

open System

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|Hit|Stand|DoubleDown|Split|Insurance|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let worked, arg' = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb ] when safeEquals verb (nameof Domain.Hit) -> Hit
    | [ verb ] when safeEquals verb (nameof Domain.Stand) -> Stand
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb;] when safeEquals verb (nameof Domain.Split) -> Split
    | [ verb;] when safeEquals verb (nameof Domain.DoubleDown) -> DoubleDown
    | [ verb;] when safeEquals verb (nameof Domain.Insurance) -> Insurance
    | _ -> ParseFailed
