open System.Net
open System.IO
open System.Collections.Generic


let http (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html


let httpViaTryFinally (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    try
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    finally
        resp.Close()


let httpViaUseBinding (url: string) =
    let req = WebRequest.Create(url)
    use resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    html


let time fn =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = fn()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")



let memoize (fn: 'T -> 'U) =
    let cache = new Dictionary<'T, 'U>(HashIdentity.Structural)
    fun n ->
        if cache.ContainsKey n then cache.[n]
        else
            let res = fn n
            cache.Add(n, res)
            res
