#load "common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let testFor () =
    for i = 1 to 10 do
        printfn "%d" i
    printfn "done"



let testSeq () =
    for (b, pj) in [("Banana 1", false); ("Banana 2", true)] do
        if pj then printfn "%s is wearing pyjamas today." b


let testEnumerator () =
    for m in Regex.Matches("All the pretty horses", "[a-zA-Z]") do
        printfn "res = %s" m.Value


type DiscreteEventCounter =
    { mutable Total: int;
      mutable Positive: int;
      Name: string }

let recordEvent (s: DiscreteEventCounter) isPositive =
    s.Total <- s.Total + 1
    if isPositive then s.Positive <- s.Positive + 1

let reportStatus (s: DiscreteEventCounter) =
    printfn "We have %d %s out of %d" s.Positive s.Name s.Total

let newCounter nm =
    { Total =0;
      Positive = 0;
      Name = nm }

let longPagecounter = newCounter "long page(s)"

let fetchLongPages url =
    let page = Common.http url
    recordEvent longPagecounter (page.Length > 10000)


let testLongPages () =
    fetchLongPages "http://www.smh.com.au" |> ignore
    fetchLongPages "http://theage.com.au" |> ignore
    reportStatus longPagecounter


let sum n m =
    let mutable res = 0
    for i = n to m do
        res <- res + i
    res


let testWhile () =
    let mutable count = 1
    while count <= 10 do
        printfn "%d" count
        count <- count + 1
    printfn "done"

let generateStamp =
    let mutable count = 0
    (fun () -> count <- count + 1; count)

let arr = [|1.0; 1.0; 1.0|]
arr.[1] <- 3.0

let arr2 = [|for i in 0..5 -> (i, i * i)|]

type ResizeArray<'T> = List<'T>
let squares = new ResizeArray<int>(seq { for i in 0 .. 10 -> i * i })

let capitals = new Dictionary<string, string>(HashIdentity.Structural)
capitals.["India"] <- "New Delhi"
capitals.["USA"] <- "Washington"

let lookupName nm (dict: Dictionary<string, string>) =
    let mutable res = ""
    let foundIt = dict.TryGetValue(nm, &res)
    if foundIt then res
    else failwithf "Didn't find %s" nm


let testException () =
    try
        raise (System.InvalidOperationException ("it's just not my day"))
    with
        :? System.InvalidOperationException -> printfn "caught"


exception BlockedURL of string

let http2 url =
    if url = "http://www.kaos.org"
    then raise (BlockedURL(url))
    else Common.httpViaUseBinding url


let testCustomException () =
    try
        http2 "http://www.kaos.org"
    with
        BlockedURL url -> sprintf "blocked! %s" url


let tmpFile = Path.Combine(__SOURCE_DIRECTORY__, "temp.txt")

let writeFile () =
    File.WriteAllLines(tmpFile, [|"Test file"; "Easy to read"|])


let readFile () =
    seq { for line in File.ReadLines tmpFile do
          let words = line.Split [|' '|]
          if words.Length > 2 && words.[0] = "Easy" then yield line }


let writeStream () =
    let outp = File.CreateText "playlist.txt"
    outp.WriteLine "Enchanted"
    outp.WriteLine "Put your records on"
    outp.Close()


let isWord (words: string list) =
    let wordTable = Set.ofList words
    fun w -> wordTable.Contains(w)

let isCapital = isWord ["London"; "Paris"; "Warsaw"; "Tokyo"]

type NameLookupService =
    abstract Contains: string -> bool

let buildSimpleNameLookup (words: string list) =
    let wordTable = HashSet<_>(words)
    {new NameLookupService with
     member t.Contains w = wordTable.Contains w}

let capitalLookup = buildSimpleNameLookup ["London"; "Paris"; "Warsaw"; "Tokyo"]

let fibFast =
    let t = new Dictionary<int, int>()
    let rec fibCached n =
        if t.ContainsKey n then t.[n]
        elif n <= 2 then 1
        else
            let res = fibCached(n - 1) + fibCached(n - 2)
            t.Add(n, res)
            res
    fun n -> fibCached n


let rec fibFast2 =
    Common.memoize (fun n -> if n <= 2 then 1 else fibFast2(n - 1) + fibFast2(n - 2))
