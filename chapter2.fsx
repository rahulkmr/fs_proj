#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "packages/Suave/lib/net40/Suave.dll"

open System.IO
open System.Net

open FSharp.Data

open Suave
open Suave.Web

let splitAtSpaces (text: string) =
    text.Split ' ' |> Array.toList

let wordCount text =
    let words = splitAtSpaces text
    let numWords = words.Length
    let distinctWords = List.distinct words
    let numDups = numWords - distinctWords.Length
    (numWords, numDups)

let showWordCount text =
    let numWords, numDups = wordCount text
    printfn "--> %d words in the text" numWords
    printfn "--> %d duplicate words" numDups

let powerOfFour n =
    let nSquared = n * n in nSquared * nSquared


let http (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

let fetchBBC () = http "http://news.bbc.co.uk"


type Species = HtmlProvider<"http://en.wikipedia.org/wiki/The_world's_100_most_threatened_species">

let species =
    [ for x in Species.GetSample().Tables.``Species list``.Rows ->
        x.Type, x.``Common name``]

let speciesSorted =
    species
        |> List.countBy fst
        |> List.sortByDescending snd


let html =
    [ yield "<html><body><ul>"
      for (category, count) in speciesSorted do
        yield sprintf "<li>Category <b>%s</b>: <b>%d</b></li>" category count
      yield "</ul></body></html>" ]
    |> String.concat "\n"

let startServer () = startWebServer defaultConfig (Successful.OK html)
