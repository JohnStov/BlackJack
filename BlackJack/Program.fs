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

[<EntryPoint>]
let main argv = 
    let deck = newDeck() |> shuffle
    for card in deck do
        printfn "%s" (renderCard card)
    let hands, deck' = deal 3 deck
    0 // return an integer exit code
