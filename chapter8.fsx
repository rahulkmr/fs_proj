#r @"packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r @"System.Xml.Linq"

open System.IO
open System.Xml
open System.Drawing
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Data.JsonExtensions

let splitLine (line: string) =
    line.Split ','  |> Array.map (fun s -> s.Trim())


let parseEmployee (line: string) =
    match splitLine line with
        | [|last; first; startDate; title|] ->
            last, first, System.DateTime.Parse(startDate), title
        | _ -> failwithf "invalid employee format: '%s'" line


let line = "Smith, John, 20 January 1992, Software Developer"

File.WriteAllLines("employee.txt", Array.create 100 line)


let readEmployees (fileName: string) =
    fileName |> File.ReadLines |> Seq.map parseEmployee

let firstThree = readEmployees "employee.txt" |> Seq.truncate 3 |> Seq.toList

firstThree |> Seq.iter (fun (last, first, startDate, title) ->
                        printfn "%s %s started on %A as '%s'" first last startDate title)



let httpRequest = "GET /favicon.ico HTTP/1.1"

let parseHttpRequest line =
    let result = Regex.Match(line, @"GET (.*?) HTTP/1\.([01])$")
    let file = result.Groups.[1].Value
    let version = result.Groups.[2].Value
    file, version


let inp = """<?xml version="1.0" encoding="utf-8" ?>
    <Scene>
        <Composite>
            <Circle radius='2' x='1' y='0'/>
            <Composite>
                <Circle radius='2' x='4' y='0'/>
                <Square side='2' left='-3' top='0'/>
            </Composite>
            <Ellipse top='2' left='-2' width='3' height='4'/>
        </Composite>
    </Scene>"""

let doc = new XmlDocument()
doc.LoadXml(inp)


type Scene =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | Composite of Scene list

    static member Circle(center: PointF, radius) =
        Ellipse(RectangleF(center.X - radius, center.Y - radius,
                           radius * 2.0f, radius * 2.0f))

    static member Square(left, top, side) =
        Rect(RectangleF(left, top, side, side))

    static member ExtractFloat32(attrName, attribs: XmlAttributeCollection)=
        float32 (attribs.GetNamedItem(attrName).Value)

    static member ExtractPointF(attribs: XmlAttributeCollection, ?nameX, ?nameY) =
        let nameX = defaultArg nameX "x"
        let nameY = defaultArg nameY "y"
        PointF(Scene.ExtractFloat32(nameX, attribs), Scene.ExtractFloat32(nameY, attribs))

    static member ExtractRectangleF(attribs: XmlAttributeCollection,
                                    ?nameLeft, ?nameTop, ?nameWidth, ?nameHeight) =
        let nameLeft = defaultArg nameLeft "left"
        let nameTop = defaultArg nameTop "top"
        let nameWidth = defaultArg nameWidth "width"
        let nameHeight = defaultArg nameHeight "height"
        RectangleF(Scene.ExtractFloat32(nameLeft, attribs),
                   Scene.ExtractFloat32(nameTop, attribs),
                   Scene.ExtractFloat32(nameWidth, attribs),
                   Scene.ExtractFloat32(nameHeight, attribs))


    static member ExtractScene(node: XmlNode) =
        let rec loop (node: XmlNode) =
            let attribs = node.Attributes
            let childNodes = node.ChildNodes
            match node.Name with
                | "Circle" ->
                    Scene.Circle(Scene.ExtractPointF(attribs),
                                 Scene.ExtractFloat32("radius", attribs))
                | "Ellipse" ->
                    Scene.Ellipse(Scene.ExtractRectangleF(attribs))
                | "Rectangle" ->
                    Scene.Rect(Scene.ExtractRectangleF(attribs))
                | "Square" ->
                    Scene.Square(Scene.ExtractFloat32("left", attribs),
                                 Scene.ExtractFloat32("top", attribs),
                                 Scene.ExtractFloat32("side", attribs))
                | "Composite" ->
                    Scene.Composite [for child in childNodes -> loop child]
                | _ ->
                    failwithf "unable to convert XML '%s'" node.OuterXml
        loop node


    static member ExtractScenes(doc: XmlDocument) =
        [for node in doc.ChildNodes do
            if node.Name = "Scene" then
                yield (Composite [for child in node.ChildNodes -> Scene.ExtractScene(child)])]




[<Literal>]
let customersXmlSample = """
<Customers>
    <Customer name="ACME">
        <Order Number="A012345">
            <OrderLine Item="widget" Quantity="1"/>
        </Order>
        <Order Number="A012346">
            <OrderLine Item="trinket" Quantity="2"/>
        </Order>
    </Customer>
    <Customer name="Southwind">
        <Order Number="A012347">
            <OrderLine Item="skyhook" Quantity="3"/>
            <OrderLine Item="gizmo" Quantity="4"/>
        </Order>
    </Customer>
</Customers>"""


type InputXml = XmlProvider<customersXmlSample>

let inputs = InputXml.GetSample().Customers


let orders =
    [ for customer in inputs do
        for order in customer.Orders do
            for line in order.OrderLines do
                yield (customer.Name, order.Number, line.Item, line.Quantity)]



let animals =
    JsonValue.Parse """
        {
            "dogs":
                [
                    {"category": "Companion dogs", "name": "Chihuahua"},
                    {"category": "Hounds", "name": "Foxhound"}
                ]
        } """

let dogs = [ for dog in animals?dogs -> dog?name ]

type Customers = JsonProvider<"""
{ "customers" :
    [ { "name" : "ACME", "orders" :
        [ { "number" : "A012345", "item" : "widget", "quantity" : 1 } ] } ] }
""">

let customers = Customers.Parse """
{ "customers" :
    [ { "name" : "Apple Store", "orders" :
        [ { "number" : "B73284",
        "item" : "iphone5",
        "quantity" : 18373 },
        { "number" : "B73238",
        "item" : "iphone6",
        "quantity" : 3736 } ] },
      { "name" : "Samsung Shop", "orders" :
        [ { "number" : "N36214",
        "item" : "nexus7",
        "quantity" : 18373 } ] } ] }
"""

let customerNames = [ for c in customers.Customers -> c.Name ]


type Term =
    | Term of int * string * int
    | Const of int

type Polynomial = Term list

type Token =
    | ID of string
    | INT of int
    | HAT
    | PLUS
    | MINUS

type TokenStream = Token list

let regex s = new Regex(s)

let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"


let tokenize (s: string) =
    [for x in tokenR.Match(s).Groups.["token"].Captures do
        let token =
            match x.Value with
            | "^" -> HAT
            | "-" -> MINUS
            | "+" -> PLUS
            | s when System.Char.IsDigit s.[0] -> INT (int s)
            | s  -> ID s
        yield token
     ]
