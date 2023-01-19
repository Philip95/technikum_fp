# Blackjack

Master Software Engineering | Functional Programming | WS2022/23

Gierer Alexandra & Strobach Philip

## Prerequisites

- .NET core SDK (as specified in `global.json`)
- Preferred F# editor

## Run the application

Execute the following command.

```bash
dotnet run src/Blackjack.fsproj
```

## Git Repository

Git Repository can be found at https://github.com/Philip95/technikum_fp

## Rationale

### Game

For this Project Blackjack was chosen. Blackjack is a card game where players try to get a hand with a higher total value than the dealer without going over 21.

For Blackjack 5 basic player options where implemented:

- Hit: A new Card is drawn to the Hand
- Stand: The Hand is ended
- DoubleDown: The Bet is doubled. The player can Hit or Stand. DoubleDown is not possible when Split is used.
- Split: The Hand is split in 2 Hands, each can now played individually. The second hand has the same bet as the initial bet. When 2 aces are split the player gets one card each, then is the round ended. It is also possible to split multiple times.
- Insurance: When the Dealer card is an ace it is possible to set an insurance. The insurance is half of the bet and paid out when the dealer has a Blackjack

The player starts with an balance of 1000. First the Player can set a bet, after that the Player gets 2 cards and the Dealer gets one. Then the player can choose any player option. Once the players turn ended, the dealer draws his card. The dealer hast to draw until a value of 16 and stand on all 17's.

Round ends:

- Blackjack: the value of the Players Hand is 21 at the beginning of the round - Blackjack is paid 3 to 2
- Bust: the value of the Players Hand is over 21 - no payout
- Push: the value of the Players Hand and of the Dealers Hand is the same - the player gets its bet back
- Win: the value of the Players Hand is closer to 21 than the Dealers Hand while not going over 21 or the dealer busts - the player receives double the bet
- Insurance: when the dealer has a Blackjack the insurance is paid out - Insurance pay 2 to 1

### Code

- Domain: domain specific values
- Game: implementation of the game
- Parser: used for Help Text
- Program: starting point of the game
- Repl: console output
