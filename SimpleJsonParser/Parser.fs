module Parser

open FParsec
open Expression

type Json = 
    | JString of string
    | JNumber of float
    | JObject of Map<string, Json>

let private forwardingParser, parserRef = createParserForwardedToRef()

let private stringLiteral =
    let allowedChars  = manySatisfy (fun c -> c <> '"' && c <> '\\')
    between (pstring "\"") (pstring "\"") allowedChars

let private jstring = stringLiteral |>> JString
let private number = pfloat |>> JNumber
let private keyword name = stringLiteral >>. pstring name |>> JString

let private betweenStrings sOpen sClose pElement f =
    between (pstring sOpen) (pstring sClose)
            (spaces >>. sepBy (pElement .>> spaces) (pstring "," .>> spaces) |>> f)

//A KeyValue is a string and an expression, separated by a colon
let private keyValue = tuple2 stringLiteral (spaces >>. pstring ":" >>. spaces >>. forwardingParser)

let private jobject = betweenStrings "{" "}" keyValue (List.map( fun(k,v) -> (k.ToLower(), v) ) >> Map.ofList >> JObject)

do parserRef := choice [ jobject; number; jstring; ]

let private json = spaces >>. forwardingParser .>> spaces .>> eof

let private readFile filePath =
    runParserOnFile json () filePath (System.Text.Encoding.UTF8)

let private tryGetMap defaultVal key map =
    match map |> Map.tryFind key with
    | Some v -> v
    | None -> defaultVal

let rec private createExpression jobj =
    match jobj with
    | JString s -> Variable(s)
    | JNumber n -> Constant(n)
    | JObject map ->
        let lhs = map.["lhs"] |> createExpression
        let rhs = map.["rhs"] |> createExpression
        let op =
            match map.["op"] with
            | JString s ->
                match s.ToLower() with
                | "add" -> Add
                | "subtract" -> Subtract
                | "multiply" -> Multiply
                | "divide" -> Divide
                | "equal" -> Equal
                | _ -> invalidArg "op" "Invalid operand value"
            | _ -> invalidArg "op" "Invalid operand value"
        Nested(lhs, op, rhs)

let private parse p =
    match p with
    | Success (v, _, _) -> createExpression v
    | Failure (msg, err, _) -> Invalid(msg)

/// <summary>
/// Parses the file in the path to a JSON AST, then an expression representation
/// </summary>
let parseFile = readFile >> parse