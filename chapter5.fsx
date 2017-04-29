open System
open System.IO
open System.Runtime.Serialization.Formatters.Binary

type Person =
    { Name: string
      DateOfBirth: DateTime }


type Point = {X: float; Y: float}
type Point3D = {X: float; Y: float; Z: float}


let p1 = {X = 3.0; Y = 4.0; Z = 5.0}
let p2 = {p1 with Y = 0.0; Z = 0.0}


type Route = int
type Make = string
type Model = string
type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route

let ian = Car("BMW", "360")
let don = [Bicycle; Bus 8]
let peter = [Car ("Ford", "Fiesta"); Bicycle]


let averageSpeed (tr: Transport) =
    match tr with
        | Car _ -> 35
        | Bicycle -> 16
        | Bus _ -> 24


type Proposition =
    | True
    | And of Proposition * Proposition
    | Or of Proposition * Proposition
    | Not of Proposition


let rec eval (p: Proposition) =
    match p with
        | True -> true
        | And(p1, p2) -> eval p1 && eval p2
        | Or(p1, p2) -> eval p1 || eval p2
        | Not(p1) -> not (eval p1)


let trueExpr = Or(Not(True), And(True, True))
let falseExpr = Or(Not(True), And(True, Not(True)))
printfn "%b" (eval trueExpr)
printfn "%b" (eval falseExpr)


type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T


let rec sizeOfTree tree =
    match tree with
        | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
        | Tip _ -> 1


let smallTree = Tree("1", Tree("2", Tip "a", Tip "b"), Tip "c")

printfn "%d" (sizeOfTree smallTree)

type Point3d = Vector3D of float * float * float
let p = Vector3D(1., 2., 3.)

let length (Vector3D(dx, dy, dz)) =
    sqrt(dx * dx + dy * dy + dz * dz)

printfn "%f" (length p)

type Node =
    { Name: string
      Links: Link list }
and Link =
    | Dangling
    | Link of Node


let rec map (f: 'T -> 'U) (l: 'T list) =
    match l with
        | [] -> []
        | h :: t -> f h :: map f t

map (fun x -> x * x) [1;2;3]
map (fun x -> int x) [1.; 2.; 3.]


let writeValue outputStream x =
    let formatter = new BinaryFormatter()
    formatter.Serialize(outputStream, box x)

let readValue inputStream =
    let formatter = new BinaryFormatter()
    let res = formatter.Deserialize(inputStream)
    unbox res


let addressList = ["Jeff", "Redmond, WA"
                   "Fred", "Pine Road, Phila"
                   "Mary", "Palo Alto, CA"]
let addresses = Map.ofList(addressList)
let fsOut = new FileStream("Data.dat", FileMode.Create)
writeValue fsOut addresses
fsOut.Close()

let fsIn = new FileStream("Data.dat", FileMode.Open)
let res: Map<string, string> = readValue fsIn
fsIn.Close()


let rec hcf a b =
    if a = 0 then b
    elif a < b then hcf a (b - a)
    else hcf (a - b) b


let hcfGeneric (zero, sub, lessThan) =
    let rec hcf a b =
        if a = zero then b
        elif lessThan a b then hcf a (sub b a)
        else hcf (sub a b) b
    hcf


let hcfInt = hcfGeneric (0, (-), (<))
let hcfLong = hcfGeneric (0L, (-), (<))
let hcfBigInt = hcfGeneric (0I, (-), (<))


type Numeric<'T> =
    { Zero: 'T
      Sub: ('T -> 'T -> 'T)
      LessThan: ('T -> 'T -> bool) }


let intOps = {Zero = 0; Sub = (-); LessThan = (<)}
let bigintOps = {Zero = 0I; Sub = (-); LessThan = (<)}
let int64Ops = {Zero = 0L; Sub = (-); LessThan = (<)}


let hcfGen (ops: Numeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan a b then hcf a (ops.Sub b a)
        else hcf (ops.Sub a b) b
    hcf


let hcfI = hcfGen intOps


type INumeric<'T> =
    abstract Zero: 'T
    abstract Sub: 'T * 'T -> 'T
    abstract LessThan: 'T * 'T -> bool


let intOpsI =
    {new INumeric<int> with
     member ops.Zero = 0
     member ops.Sub(x, y) = x - y
     member ops.LessThan(x, y) = x < y }


let hcfGenI (ops: INumeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan(a, b) then hcf a (ops.Sub(b, a))
        else hcf (ops.Sub(a, b)) b
    hcf


let hcfIntI = hcfGenI intOpsI
