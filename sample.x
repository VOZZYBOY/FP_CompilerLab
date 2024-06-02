#ast 
type Expr =
    | Num of int
    | Var of string
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Func of string * Expr
    | Call of Expr * Expr
    | Op of string * Expr * Expr



# parser open FParsec

let parseNumber = pint32 |>> Num

let parseVariable = many1SatisfyL isLetter "variable" |>> Var

let parseExpr, parseExprRef = createParserForwardedToRef<Expr, unit>()

let parseLet =
    between (pstring "(") (pstring ")") (
        pstring "let" >>. spaces1 >>.
        many1SatisfyL isLetter "variable" .>> spaces1 >>= fun var ->
        parseExpr .>> spaces1 >>= fun value ->
        parseExpr .>> spaces1 >>= fun body ->
        preturn (Let (var, value, body))
    )

let parseIf =
    between (pstring "(") (pstring ")") (
        pstring "if" >>. spaces1 >>
        parseExpr .>> spaces1 >>= fun cond ->
        parseExpr .>> spaces1 >>= fun thenBranch ->
        parseExpr .>> spaces1 >>= fun elseBranch ->
        preturn (If (cond, thenBranch, elseBranch))
    )

let parseFunc =
    between (pstring "(") (pstring ")") (
        pstring "func" >>. spaces1 >>
        many1SatisfyL isLetter "variable" .>> spaces1 >>= fun param ->
        parseExpr .>> spaces1 >>= fun body ->
        preturn (Func (param, body))
    )

let parseCall =
    between (pstring "(") (pstring ")") (
        parseExpr .>> spaces1 >>= fun func ->
        parseExpr .>> spaces1 >>= fun arg ->
        preturn (Call (func, arg))
    )

let parseOp =
    between (pstring "(") (pstring ")") (
        many1SatisfyL isLetter "operator" .>> spaces1 >>= fun op ->
        parseExpr .>> spaces1 >>= fun left ->
        parseExpr .>> spaces1 >>= fun right ->
        preturn (Op (op, left, right))
    )

do parseExprRef := choice [
    parseNumber
    parseVariable
    parseLet
    parseIf
    parseFunc
    parseCall
    parseOp
]

let parse input = run parseExpr input

#interpretur


open System.Collections.Generic

type Value =
    | Int of int
    | Closure of string * Expr * Env
and Env = Dictionary<string, Value>

let rec eval (env: Env) (expr: Expr): Value =
    match expr with
    | Num n -> Int n
    | Var x -> env.[x]
    | Let (x, valueExpr, body) ->
        let value = eval env valueExpr
        env.Add(x, value)
        eval env body
    | If (cond, thenBranch, elseBranch) ->
        match eval env cond with
        | Int 0 -> eval env elseBranch
        | Int _ -> eval env thenBranch
        | _ -> failwith "Condition must be an integer"
    | Func (param, body) -> Closure (param, body, env)
    | Call (funcExpr, argExpr) ->
        match eval env funcExpr with
        | Closure (param, body, closureEnv) ->
            let argValue = eval env argExpr
            let newEnv = Dictionary<string, Value>(closureEnv)
            newEnv.Add(param, argValue)
            eval newEnv body
        | _ -> failwith "Function expected"
    | Op (op, left, right) ->
        let lval = eval env left
        let rval = eval env right
        match (op, lval, rval) with
        | ("add", Int l, Int r) -> Int (l + r)
        | ("sub", Int l, Int r) -> Int (l - r)
        | ("mul", Int l, Int r) -> Int (l * r)
        | ("div", Int l, Int r) -> Int (l / r)
        | ("eq", Int l, Int r) -> Int (if l = r then 1 else 0)
        | _ -> failwith "Unknown operator or mismatched types"

let runProgram program =
    let env = Dictionary<string, Value>()
    match parse program with
    | Success (result, _, _) -> eval env result
    | Failure (err, _, _) -> failwith err



#пример использование просто алгоритм поиска факториала 
let factorialProgram = "
(let fact
  (func n
    (if (eq n 0)
        1
        (mul n (fact (sub n 1)))))
  (fact 5))
"

let result = runProgram factorialProgram
