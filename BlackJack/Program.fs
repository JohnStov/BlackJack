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
    String.Format("{0} of {1}", toString(card.value),  toString(card.suit))

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
            | Soft(a), Soft(b) | Hard(a), Soft(b) | Soft(a), Hard(b) -> Soft(a + b)
            | Hard(a), Hard(b) -> Hard(a + b)
            | Bust(a), Soft(b) | Bust(a), Hard(b) | Bust(a), Bust(b)| Soft(a), Bust(b) | Hard(a), Bust(b) -> Bust(a + b)
            | Blackjack, _ | _, Blackjack -> Blackjack
        
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

[<EntryPoint>]
let main argv = 
    let deck = newDeck() |> shuffle
    for card in deck do
        printfn "%s" (renderCard card)
    let hands, deck' = deal 26 deck
    let scores = hands |> List.map (fun elem -> calculateScore elem)
    0 // return an integer exit code
