module Domain

let INITAL_CASH = 1000
let BLACKJACK = 21
let DEALER_HIGHEST_SOFT_HIT = 16
let PAYOUT_FACTOR = 2.0
let BLACKJACK_PAYOUT_FACTOR = 2.5
let INSURANCE_PAYOUT_FACTOR = 3.0

type Suit = Clubs | Diamonds | Hearts | Spades
type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Card = { Suit: Suit; Rank: Rank }
type Hand = Card list

type Player = { Balance: float; mutable Hands: Hand list }
type Dealer = { mutable Hand: Hand }

type State = { Player: Player; Dealer: Dealer; Bet: float; Insurance: float; currentHand: int }
type Message =
    | Hit
    | Stand
    | DoubleDown
    | Split
    | Insurance

let init () : State =
    { Player = { Balance = INITAL_CASH; Hands = [] }; Dealer = { Hand = [] }; Bet = 0; Insurance = 0; currentHand = 0 }

let value card =
    match card.Rank with
    | Ace -> 11
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 10
    | Queen -> 10
    | King -> 10

type payoutOption = Blackjack | Push | Win | Insurance | Loose

let payout value payoutOption =
    match payoutOption with
    | Blackjack -> value * BLACKJACK_PAYOUT_FACTOR
    | Push -> value
    | Win -> value * PAYOUT_FACTOR
    | Insurance -> value * INSURANCE_PAYOUT_FACTOR
    | Loose -> 0.0

