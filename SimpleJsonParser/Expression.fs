module Expression
 
open System
open Helper
 
type Operation =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Equal
 
type Expression =
    | Constant of float
    | Variable of string
    | Nested of Expression * Operation * Expression
    | Invalid of string
 
type SimplifiedExpression = string*Expression
 
let private flip (op: Operation) : Operation =
    match op with
    | Add -> Subtract
    | Subtract -> Add
    | Multiply -> Divide
    | Divide -> Multiply
    | Equal -> Equal
 
/// <summary>
/// Produces a pretty-print string for an expression 'exp'
/// </summary>
/// <param name="exp">The expression to produce the string for.</param>
let rec prettyPrint (exp: Expression) : string =
    match exp with
    | Constant c -> Convert.ToString(c)
    | Variable x -> x
    | Nested (lhs, op, rhs) ->
        let lhs' = prettyPrint lhs
        let op' =
            match op with
            | Add -> "+"
            | Subtract -> "-"
            | Multiply -> "*"
            | Divide -> "/"
            | Equal -> "="
        let rhs' = prettyPrint rhs
        sprintf "(%s %s %s)" lhs' op' rhs'
    | Invalid msg -> msg
 
/// <summary>
/// Simplifies an expression 'exp' to be of the form (Variable = Expression).
/// Throws an exception if the expression is not simplifiable.
/// </summary>
/// <param name="exp">The expression to simplify</param>
let simplify exp : SimplifiedExpression =
    let rec toBeBalanced e acc =
        match e with
        | Constant(_) -> acc || false
        | Variable(_) -> acc || true
        | Nested(l,_,r) -> acc || (toBeBalanced l acc) || (toBeBalanced r acc)
        | Invalid(_) -> false

    let rec simplify' toBalance prevop (var,constructing) =
        match toBalance with
            | Nested(lhs, op, rhs) ->
                // non-commutative operators
                match op with
                | Subtract
                | Divide ->
                    let v1,fromRhs = simplify' rhs op (var,constructing)
                    match String.IsNullOrEmpty(v1) with
                    | true ->
                        let constructing' = Nested(constructing, flip op, rhs)
                        simplify' lhs op (v1,constructing')
                    | false ->
                        let constructing' = Nested(lhs, op, constructing)
                        simplify' rhs (flip op) (v1,constructing')
                | _ ->
                    match toBeBalanced lhs false with
                    | true -> 
                        let constructing' = Nested(constructing, flip op, rhs)
                        simplify' lhs op (var,constructing')
                    | false ->
                        let v1,fromLhs = simplify' lhs prevop (var,constructing)
                        let v2,fromRhs = simplify' rhs op (v1,fromLhs)
                        (coalesceString [v1; v2; var;]), fromRhs
            | Constant(_) -> (var, Nested(constructing, (flip prevop), toBalance))
            | Variable(v) -> (v, constructing)
            | Invalid(msg) -> (msg,toBalance)
  
    match exp with
        | Nested(lhs, Equal, rhs) -> (simplify' lhs Add ("", rhs))
        | _ -> invalidOp "Invalid expression"
 
/// <summary>
/// Solves a simplified expression 'exp'.
/// Throws an exception if the expression is not simplified.
/// </summary>
/// <param name="exp">A simplified expression of the form (Variable = Expression)</param>
let solve (exp: SimplifiedExpression) =
    let rec solve' e =
        match e with
        | Nested(lhs, op, rhs) ->
            match op with
                | Add -> (solve' lhs) + (solve' rhs)
                | Subtract -> (solve' lhs) - (solve' rhs)
                | Multiply -> (solve' lhs) * (solve' rhs)
                | Divide -> (solve' lhs) / (solve' rhs)
                | Equal -> invalidOp "Cannot apply Equal on the operands"
        | Constant(c) -> c
        | _ -> invalidOp "Invalid expression"
 
    Nested(Variable(fst exp), Equal, Constant(solve' (snd exp)))