type Expr =
    | Var of string
    | Int of int
    | Add of Expr * Expr
    | Mul of Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr

let tokenize input =
    input.Split(['('; ')'; ' '; '\n'; '\t'], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let rec parseExpr tokens =
    match tokens with
    | "(" :: "let" :: var :: "=" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Let(var, expr1, expr2), tokens''
    | "(" :: "add" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Add(expr1, expr2), tokens''
    | "(" :: "mul" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Mul(expr1, expr2), tokens''
    | "(" :: "if" :: rest ->
        let cond, tokens' = parseExpr rest
        let expr1, tokens'' = parseExpr tokens'
        let expr2, tokens''' = parseExpr tokens''
        If(cond, expr1, expr2), tokens'''
    | num :: rest when System.Int32.TryParse(num).IsSuccess ->
        Int(int num), rest
    | var :: rest -> Var(var), rest

let parse input = 
    let tokens = tokenize input
    fst (parseExpr tokens)

let rec eval env expr =
    match expr with
    | Int n -> n
    | Add(e1, e2) -> eval env e1 + eval env e2
    | Mul(e1, e2) -> eval env e1 * eval env e2
    | Var name -> Map.find name env
    | Let(name, e1, e2) ->
        let value = eval env e1
        let newEnv = Map.add name value env
        eval newEnv e2
    | If(cond, e1, e2) ->
        if eval env cond <> 0 then eval env e1 else eval env e2

let program = "(let x = (add 5 10) (if (mul x 2) (add x 5) 0))"

let main () =
    let expr = parse program
    let result = eval Map.empty expr
    printfn "Result: %d" result

main ()


// primer 
(let x = (add 5 10) (if (mul x 2) (add x 5) 0))

