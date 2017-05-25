open System
open System.IO


let range = seq {0 .. 2 .. 6}
for i in range do printfn "i = %d" i


let rec allFiles dir =
    Seq.append
        (dir |> Directory.GetFiles)
        (dir |> Directory.GetDirectories |> Seq.map allFiles |> Seq.concat)


let squares = seq { for i in 0 .. 10 -> (i, i * i) }


let checkerboardConditions n =
    seq { for row in 1 .. n do
            for col in 1 .. n do
                let sum = row + col
                if sum % 2 = 0 then
                    yield (row, col) }


let fileInfo dir =
    seq { for file in Directory.GetFiles dir do
            let creationTime = File.GetCreationTime file
            let lastAccessTime = File.GetLastAccessTime file
            yield (file, creationTime, lastAccessTime) }



let rec allFilesSeq dir =
    seq { for file in Directory.GetFiles dir do
            yield file
          for subdir in Directory.GetDirectories dir do
            yield! allFilesSeq dir }


let people =
    [("Amber", 27, "Design")
     ("Wendy", 35, "Events")
     ("Antonio", 40, "Sales")
     ("Petra", 31, "Design")
     ("Carlos", 34, "Marketing")]


let namesOfPeopleStartingWithA =
    people
    |> Seq.map (fun (name, _age, _dept) -> name)
    |> Seq.filter (fun name -> name.StartsWith "A")
    |> Seq.toList


let namesOfDesigners =
    people
    |> Seq.filter (fun (_, _, dept) -> dept = "Design")
    |> Seq.map (fun (name, _, _) -> name)
    |> Seq.toList


let rand = System.Random()
let randomNumbers = seq { while true do yield rand.Next(1000) }

let firstTenRandomNumbers =
    randomNumbers
    |> Seq.truncate 10
    |> Seq.sort
    |> Seq.toList


let firstFiftyEvenNumbers =
    randomNumbers
    |> Seq.filter (fun i -> i % 2 = 0)
    |> Seq.truncate 50
    |> Seq.sort
    |> Seq.toList


let triangleNumbers =
    [ 1 .. 10 ]
    |> Seq.collect (fun i -> [ 1 .. i ])
    |> Seq.toList


let triangleNumbers2 =
    [ for i in 1 .. 10 do
        for j in 1 .. i do
            yield (i, j) ]

let gameBoard =
    [ for i in 0 .. 7 do
        for j in 0 .. 7 do
            yield (i, j, rand.Next(10)) ]


let evenPositions =
    gameBoard
    |> Seq.choose (fun (i, j, v) -> if v % 2 = 0 then Some (i, j) else None)
    |> Seq.toList


let positionsGroupedByGameValue =
    gameBoard
    |> Seq.groupBy (fun (i, j, v) -> v)
    |> Seq.sortBy (fun (k, v) -> k)
    |> Seq.toList



let positionsIndexedByValue =
    gameBoard
    |> Seq.groupBy (fun (i, j, v) -> v)
    |> Seq.sortBy (fun (k, v) -> k)
    |> Seq.map (fun (k, v) -> (k, Seq.toList v))
    |> dict


let sum = Seq.fold (fun acc x -> acc + x) 0 [1 .. 10]
let sum2 = Seq.fold (+) 0 [1 .. 10]
let sum3 = (0, [1 .. 10]) ||> List.fold (+)
