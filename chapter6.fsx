open System.Collections.Generic
open System.Drawing
open System.Text
open System.IO

type Vector2D =
    { DX: float; DY: float }

    member this.Length = sqrt(this.DX * this.DX + this.DY * this.DY)

    member this.Scale k = { DX = k * this.DX; DY = k * this.DY }

    member this.ShiftX x = { this with DX = this.DX + x }

    member this.ShiftY y = { this with DY = this.DY + y }

    member this.ShiftXY (x, y) = { DX = this.DX + x; DY = this.DY + y }

    static member Zero = { DX = 0.0; DY = 0.0 }

    static member ConstX dx = { DX = dx; DY = 0.0 }

    static member ConstY dy = { DX = 0.0; DY = dy }


 type Tree<'T> =
     | Node of 'T * Tree<'T> * Tree<'T>
     | Tip

     member this.Size =
         match this with
             | Node(_, l, r) -> 1 + l.Size + r.Size
             | Tip -> 0



type Vector(dx: float, dy: float) =
    let len = sqrt(dx * dx + dy * dx)
    static let zero = Vector(0.0, 0.0)
    static let onex = Vector(1.0, 0.0)
    static let oney = Vector(0.0, 1.0)

    member this.DX = dx
    member this.DY = dy
    member this.Length = len
    member this.Scale(k) = Vector(k * dx, k * dy)
    member this.ShiftX(x) = Vector(dx = dx + x, dy = dy)
    member this.ShiftY(y) = Vector(dx = dx, dy = dy + y)
    member this.ShiftXY(x, y) = Vector(dx = dx + x, dy = dy + y)
    static member Zero = zero
    static member OneX = onex
    static member OneY = oney

    static member (+) (v1: Vector, v2: Vector) =
        Vector(v1.DX + v2.DX, v1.DY + v2.DY)

    static member (-) (v1: Vector, v2: Vector) =
        Vector(v1.DX - v2.DX, v1.DY - v2.DY)


type UnitVector(dx, dy) =
    let tolerance = 0.000001
    let length = sqrt(dx * dx + dy * dy)
    do if abs (length - 1.0) >= tolerance then failwith "not a unit vector";

    member this.DX = dx
    member this.DY = dy
    new() = UnitVector(1.0, 0.0)



type SparseVector(items: seq<int * float>) =
    let elems = new SortedDictionary<_, _>()
    do items |> Seq.iter (fun (k, v) -> elems.Add(k, v))

    member t.Item
        with get(idx) =
            if elems.ContainsKey(idx) then elems.[idx]
            else 0.0




type LabelInfo(?text: string, ?font: Font) =
    let text = defaultArg text ""
    let font = match font with
                | None -> new Font(FontFamily.GenericSansSerif, 12.0f)
                | Some v -> v
    member x.Text = text
    member x.Font = font

    static member Create(?text, ?font) = new LabelInfo(?text=text, ?font=font)



type Interval(lo, hi) =
    member r.Lo = lo
    member r.Hi = hi
    member r.IsEmpty = hi <= lo
    member r.Contains v = lo < v && v < hi

    static member Empty = Interval(0.0, 0.0)

    static member Span (r1: Interval, r2: Interval) =
        if r1.IsEmpty then r2
        elif r2.IsEmpty then r1
        else Interval(min r1.Lo r2.Lo, max r1.Hi r2.Hi)

    static member Span (ranges: seq<Interval>) =
        Seq.fold (fun r1 r2 -> Interval.Span(r1, r2)) Interval.Empty ranges



type MyPoint =
    { X: float; Y: float }

    static member (-) (p1: MyPoint, p2: MyPoint) =
        { DX = p1.X - p2.X; DY = p1.Y - p2.Y }


    static member (-) (p: MyPoint, v: Vector) =
        { X = p.X - v.DX; Y = p.Y - v.DY }



type MutableVector(dx: float, dy: float) =
    let mutable currDX = dx
    let mutable currDY = dy

    member this.DX
        with get () = currDX
        and set v = currDX <- v

    member this.DY
        with get () = currDY
        and set v = currDY <- v

    member this.Length
        with get () = sqrt (currDX * currDX + currDY * currDY)
        and set len =
            let theta = this.Angle
            currDX <- cos theta * len
            currDY <- sin theta * len

    member this.Angle
        with get () = atan2 currDY currDX
        and set theta =
            let len = this.Length
            currDX <- cos theta * len
            currDY <- sin theta * len





type IShape =
    abstract Contains: Point -> bool
    abstract BoundingBox: Rectangle


let circle (center: Point, radius: int) =
    { new IShape with
      member this.Contains (p: Point) =
        let dx = float32 (p.X - center.X)
        let dy = float32 (p.Y - center.Y)
        sqrt(dx * dx + dy * dy) <= float32 radius

      member this.BoundingBox =
        Rectangle(center.X - radius, center.Y - radius, 2 * radius + 1,
                  2 * radius + 1)
      }



let square (center: Point, side: int) =
    { new IShape with

      member this.Contains (p: Point) =
        let dx = p.X - center.X
        let dy = p.Y - center.Y
        (abs dx) < side / 2 && (abs dy) < side / 2


      member this.BoundingBox =
        Rectangle(center.X - side, center.Y - side, side * 2, side * 2)
      }


type MutableCircle () =
    member val Center = Point(x = 0, y = 0) with get, set
    member val Radius = 10 with get, set

    member c.Perimeter = 2.0 * System.Math.PI

    interface IShape with
        member this.Contains (p: Point) =
            let dx = float32 (p.X - this.Center.X)
            let dy = float32 (p.Y - this.Center.Y)
            sqrt(dx * dx + dy * dy) <= float32 this.Radius

        member this.BoundingBox =
            Rectangle(this.Center.X - this.Radius, this.Center.Y - this.Radius,
                      2 * this.Radius + 1, 2 * this.Radius + 1)



[<AbstractClass>]
type TextOutputSink () =
    abstract WriteChar: char -> unit
    abstract WriteString: string -> unit
    default this.WriteString s = s |> String.iter this.WriteChar



type HtmlWriter () =
    let mutable count = 0
    let sink =
        { new TextOutputSink() with
          member this.WriteChar c =
            count <- count + 1
            System.Console.Write c
          }

    member this.CharCount = count
    member this.OpenTag(tagName) = sink.WriteString(sprintf "<%s>" tagName)
    member this.CloseTag(tagName) = sink.WriteString(sprintf "</%s>" tagName)
    member this.WriteString(s) = sink.WriteString(s)



type CountingByInheritance () =
    inherit TextOutputSink()

    let mutable count = 0

    member this.Count = count

    override this.WriteChar c =
        count <- count + 1
        System.Console.Write c



[<AbstractClass>]
type ByteOutputSink () =
    inherit TextOutputSink()

    abstract WriteByte: byte -> unit
    abstract WriteBytes: byte [] -> unit

    override this.WriteChar c = this.WriteBytes(Encoding.UTF8.GetBytes [|c|])
    override this.WriteString s = this.WriteBytes(Encoding.UTF8.GetBytes s)

    default this.WriteBytes b = b |> Array.iter this.WriteByte



type LineChooser(fileName1, fileName2) =
    let file1 = File.OpenText(fileName1)
    let file2 = File.OpenText(fileName2)
    let rnd = new System.Random()

    let mutable disposed = false

    let cleanup() =
        if not disposed then
            disposed <- true;
            file1.Dispose();
            file2.Dispose();

    interface System.IDisposable with
        member this.Dispose() = cleanup()

    member this.CloseAll() = cleanup()

    member this.GetLine() =
        if not file1.EndOfStream &&
            (file2.EndOfStream || rnd.Next() % 2 = 0) then file1.ReadLine()
        elif not file2.EndOfStream then file2.ReadLine()
        else raise (new EndOfStreamException())



module NumberExtensions =
    let factorize i =
        let lim = int (sqrt (float i))
        let rec check j =
            if j > lim then None
            elif (i % j) = 0 then Some (i / j, j)
            else check (j + 1)
        check 2

    type System.Int32 with
        member this.IsPrime = (factorize this).IsNone
        member this.TryFactorize() = factorize this
