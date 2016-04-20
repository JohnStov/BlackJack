open Microsoft.FSharp.Reflection
open System

let rand = new System.Random()

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

type Suit = Spades | Hearts | Diamonds | Clubs
type Value =
    | Ace
    | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King

type Card = { value : Value; suit : Suit }
    
type Deck = Card list

type Hand = Card list

type Score =
    | Bust of int
    | Blackjack
    | Hard of int
    | Soft of int
    | Stand of int

type Player = {
    name : string;
    hand : Hand;
    score : Score;
}

type Game = {
    deck : Deck;
    players : Player list;
    player : int;
}

let allValues<'T> () =
    FSharpType.GetUnionCases(typeof<'T>)
    |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)

let toString (x : 'T) =
    match FSharpValue.GetUnionFields(x, typeof<'T>) with
    | case, _ -> case.Name

let newDeck() : Deck = 
    [for suit in allValues<Suit>() do
        for value in allValues<Value>() do
            yield {value = value; suit = suit}]

let shuffle (d : Deck) =
    let a = d |> List.toArray
    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a 
    a |> Array.toList

let renderCard (card : Card) =
    printfn "%s of %s" (toString(card.value))  (toString(card.suit))

let takeTopCard (deck : Deck) : Card * Deck =
    match deck with
    | [] -> failwith "Out of cards"
    | top :: deck' -> top, deck'

let deal playerCount (deck : Deck) : Hand list * Deck =
    let dealRound count (deck : Deck)=
        let rec dealAccumulate count (deck: Deck) (cards : Card list) = 
            match count with 
            | 0 -> (cards, deck)
            | _ -> 
                let (topCard, newDeck) = takeTopCard deck
                dealAccumulate (count - 1) newDeck (List.append cards [topCard])
        dealAccumulate count deck []

    match playerCount with
    | a when( a < 2) -> failwith "Too few players"
    | a when a > (deck.Length / 2) -> failwith "Too many players"
    | _ -> 
        let (firstRound, deck') = dealRound playerCount deck
        let (secondRound, deck') = dealRound playerCount deck'
        let hands = List.map2 (fun item1 item2 -> [item1 ; item2]) firstRound secondRound
        hands, deck'

let (|Face|_|) value =
    match value with
    | Ten | Jack | Queen | King -> Some Face
    | _ -> None

let calculateScore (hand : Hand) =
    let cardScore (card : Card) = 
        match card.value with 
        | Ace -> Soft(1)
        | Two -> Hard(2)
        | Three -> Hard(3)
        | Four -> Hard(4)
        | Five -> Hard(5)
        | Six -> Hard(6)
        | Seven -> Hard(7)
        | Eight -> Hard(8)
        | Nine -> Hard(9)
        | _ -> Hard(10)

    let addScores (left : Score) (right : Score) =
        let innerAdd (left : Score) (right : Score) = 
            match left, right with
            | Soft a , Soft b | Hard a , Soft b | Soft a , Hard b -> Soft (a + b)
            | Hard a , Hard b -> Hard (a + b)
            | Bust a , Soft b | Bust a , Hard b | Bust a , Bust b| Soft a , Bust b | Hard a , Bust b -> Bust (a + b)
            | Blackjack , _ | _ , Blackjack -> Blackjack
            | Stand n , _ | _ , Stand n -> Stand n
        
        let result = innerAdd left right
        match result with 
        | Soft(a) when a > 21 -> Bust(a) 
        | Hard(a) when a > 21 -> Bust(a)
        | _ -> result
        
    if hand.Length = 2 then
        match hand.[0].value, hand.[1].value with
        | Ace, Face | Face, Ace -> Blackjack
        | _, _ -> addScores (cardScore hand.[0]) (cardScore hand.[1])
    else
        hand |> List.fold (fun acc elem -> addScores acc (cardScore elem)) (Hard(0))

let renderScore (score : Score) =
    match score with
    | Soft n -> printfn "%d or %d" n (n + 10) 
    | Hard n -> printfn "%d" n
    | Bust n -> printfn "Bust! (%d)" n
    | Blackjack  -> printfn "Blackjack!"
    | Stand n  -> printfn "Stand at %d" n

let createGame playerCount = 
    let deck = newDeck() |> shuffle
    let hands, deck = deal playerCount deck
    let scores = hands |> List.map (fun hand -> calculateScore hand)
    let players = [for playerNum in [0 .. (playerCount - 1)] do 
                        yield {
                            hand = hands.[playerNum]; 
                            score = scores.[playerNum]; 
                            name = if playerNum = (playerCount - 1) then "Dealer" else (String.Format ("Player {0}", (playerNum + 1)))
                        }]
    { deck = deck; players = players; player = 0 }

let displayScore (player : Player) =
    printfn "%s" player.name
    for card in player.hand do
        renderCard card
    renderScore player.score

let displayPlayerScore (game : Game) =
    game.players.[game.player] |> displayScore

let displayScores (game : Game) =
    for player in game.players do
        player |> displayScore

let finished (game : Game) =
    game.player >= game.players.Length

let stand (game : Game) =
    { game with player = (game.player + 1) }

let hit (game : Game) = 
    let hitPlayer (player : Player) (deck : Deck) =
        let card, deck = deck |> takeTopCard
        let newHand = card :: player.hand
        let newScore = calculateScore newHand 
        { player with hand = newHand; score = newScore }, deck
    
    let newPlayer, deck = hitPlayer game.players.[game.player] game.deck
    let players = game.players |> List.mapi (fun index player -> if index = game.player then newPlayer else player)
    {game with players = players; deck = deck}


[<EntryPoint>]
let main argv = 
    let game = createGame 3
    game |> displayScores
    let game = game |> hit
    game |> displayPlayerScore
    0 // return an integer exit code
