type Suit = Spades | Hearts | Diamonds | Clubs
type Face = Jack | Queen | King
type Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten

type Value =
    | Ace
    | Face of Face
    | Pip of Pip

type Card = { value : Value; suit : Suit }
    
let aceOfSpades = {value = Ace; suit = Spades}

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
