#load "common.fsx"

open System
open System.Drawing
open System.IO

let round x =
    if x >= 100 then 100
    elif x < 0 then 0
    else x


let round2 x =
    match x with
    | _ when x >= 100 -> 100
    | _ when x < 0 -> 0
    | _ -> x


let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)

let rec length l =
    match l with
    | [] -> 0
    | h :: t -> 1 + length t


let rec even n = (n = 0u) || odd(n - 1u)
and odd n = (n <> 0u) && even(n - 1u)


let oddPrimes = [3;5;7;11]
let morePrimes = [13;17]
let primes = 2 :: (oddPrimes @ morePrimes)

let people =
    [("Adam", None);
     ("Eve", None);
     ("Cain", Some("Adam", "Eve"));
     ("Abel", Some("Adam", "Eve"))]


let fetch url =
    try Some (Chapter2.http url)
    with :? System.Net.WebException -> None

let testFetch () =
    match (fetch "http://www.nature.com") with
        | Some text -> printfn "text = %s" text
        | None -> printfn "*** no web page found"


let printFirst xs =
    match xs with
        | h :: t -> printfn "The first item in the list is %A" h
        | [] -> printfn "No items in the list"


let showParents (name, parents) =
    match parents with
        | Some (dad, mum) -> printfn "%s has father %s and mother %s" name dad mum
        | None -> printfn "%s has no parents" name

let testParents () =
    for person in people do showParents person


let urlFilter url agent =
    match url, agent with
        | "http://www.control.org", 86 -> true
        | "http://www.kaos.org", _ -> true
        | _ -> false


let sign x =
    match x with
        | _ when x < 0 -> -1
        | _ when x > 0 -> 1
        | _ -> 0


let sites = ["http://www.bing.com"; "http://www.google.co.in"]

let delimiters = [| ' '; '\n'; '\t'; '<'; '>'; '=' |]
let getWords (s: string) = s.Split delimiters
let getStats site =
    let url = site
    let html = Chapter2.http url
    let hwords = html |> getWords
    let hrefs = html |> getWords |> Array.filter (fun s -> s = "href")
    (site, html.Length, hwords.Length, hrefs.Length)



let remap (r1: RectangleF) (r2: RectangleF) =
    let scalex = r2.Width / r1.Width
    let scaley = r2.Height / r1.Height
    let mapx x = r2.Left + (x - r1.Left) * scalex
    let mapy y = r2.Top + (y - r1.Top) * scaley
    let mapp (p:PointF) = PointF(mapx p.X, mapy p.Y)
    mapp

let rect1 = RectangleF(100.0f, 100.0f, 100.0f, 100.0f)
let rect2 = RectangleF(50.0f, 50.0f, 200.0f, 200.0f)

let mapp = remap rect1 rect2

let timeFunc f =
    let start = DateTime.Now
    let res = f()
    let finish = DateTime.Now
    (res, finish - start)

let testTimeFunc () =
    timeFunc (fun () -> Common.http "http://newscientist.com")


let testObjectMethods () =
    ["file1.txt"; "file2.js"; "file3.fs"] |> List.map Path.GetExtension
