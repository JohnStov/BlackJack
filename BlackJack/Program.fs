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

[<EntryPoint>]
let main argv = 
    let deck = newDeck() |> shuffle
    for card in deck do
        printfn "%s" (renderCard card)
    0 // return an integer exit code
