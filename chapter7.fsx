type IPeekPoke =
    abstract member Peek: unit -> int
    abstract member Poke: int -> unit


let makeCounter initialState =
    let mutable state = initialState
    { new IPeekPoke with
      member this.Poke n = state <- state + n
      member this.Peek() = state }


type TicketGenerator() =
    let mutable count = 0

    member this.Next() =
        count <- count + 1
        count

    member this.Reset() =
        count <- 0
