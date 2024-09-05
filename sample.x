// Определение AST
type Expr =
    | Var of string                // Переменные
    | Int of int                   // Целочисленные значения
    | Lambda of string * Expr       // Лямбда-функции
    | App of Expr * Expr            // Применение функций
    | LetRec of string * Expr * Expr // Рекурсивное определение
    | If of Expr * Expr * Expr      // Условные выражения
    | BinOp of string * Expr * Expr // Бинарные операции

// Определение окружения
type Env = (string * Expr) list

// Функция вычисления (интерпретатор)
let rec eval (expr: Expr) (env: Env) : Expr =
    match expr with
    | Var x -> 
        match List.tryFind (fun (name, _) -> name = x) env with
        | Some (_, v) -> v
        | None -> failwithf "Переменная %s не найдена" x
    
    | Int v -> Int v
    
    | Lambda(arg, body) -> Lambda(arg, body)
    
    | App(func, arg) ->
        let funcEval = eval func env
        let argEval = eval arg env
        match funcEval with
        | Lambda(argName, body) ->
            let newEnv = (argName, argEval) :: env
            eval body newEnv
        | _ -> failwith "Не является функцией"
    
    | LetRec(name, valueExpr, body) ->
        let recEnv = (name, eval valueExpr env) :: env
        eval body recEnv
    
    | BinOp(op, left, right) ->
        let leftEval = eval left env
        let rightEval = eval right env
        match (leftEval, rightEval) with
        | (Int l, Int r) ->
            match op with
            | "+" -> Int (l + r)
            | "-" -> Int (l - r)
            | "*" -> Int (l * r)
            | "/" -> Int (l / r)
            | "=" -> if l = r then Int 1 else Int 0
            | _ -> failwith "Неподдерживаемая операция"
        | _ -> failwith "Операнды должны быть числами"
    
    | If(cond, thenBranch, elseBranch) ->
        let condEval = eval cond env
        match condEval with
        | Int 0 -> eval elseBranch env
        | _ -> eval thenBranch env

// Пример 1: Функция факториала
let factorialProgram = 
    LetRec("fact", 
        Lambda("n", 
            If(BinOp("=", Var "n", Int 0), 
               Int 1, 
               BinOp("*", Var "n", App(Var "fact", BinOp("-", Var "n", Int 1)))
            )
        ),
        App(Var "fact", Int 5)
    )

let result1 = eval factorialProgram []
printfn "Факториал 5: %A" result1  // Ожидаемый результат: Int 120

// Пример 2: Лямбда-функция и её применение
let lambdaExample = App(Lambda("x", BinOp("+", Var "x", Int 1)), Int 3)

let result2 = eval lambdaExample []
printfn "Лямбда пример: %A" result2  // Ожидаемый результат: Int 4

// Пример 3: Числа Фибоначчи
let fibonacciProgram = 
    LetRec("fib", 
        Lambda("n", 
            If(BinOp("=", Var "n", Int 0), 
               Int 0, 
               If(BinOp("=", Var "n", Int 1), 
                  Int 1, 
                  BinOp("+", App(Var "fib", BinOp("-", Var "n", Int 1)), App(Var "fib", BinOp("-", Var "n", Int 2)))
               )
            )
        ),
        App(Var "fib", Int 10)
    )

let result3 = eval fibonacciProgram []
printfn "Фибоначчи 10: %A" result3  // Ожидаемый результат: Int 55

