FuncLang
Описание проекта
Этот проект представляет собой реализацию простого функционального языка программирования под названием FuncLang. Язык поддерживает основные концепции функционального программирования, такие как рекурсия, замыкания и ленивые вычисления. Мы разработали синтаксис, парсер и интерпретатор для этого языка на языке программирования F#.Коды находятся в файле samples.В работе пользовался ии от гугла чат гпт и яндексом так как  я один в команде и тяжко одному такое делать)

Функциональность языка
FuncLang поддерживает следующие возможности:

Именованные переменные (let): создание переменной и использование её в вычислениях.
Рекурсия: функции могут вызывать сами себя.
Функции: создание и вызов функций с параметрами.
Замыкания: функции сохраняют окружение, в котором они были созданы.
Условные выражения (if): ветвление исполнения программы на основе условия.
Арифметические операции: сложение, вычитание, умножение, деление и сравнение.
Примеры программ
Факториал числа
Программа для вычисления факториала числа 5 выглядит так:

fsharp
(let fact
  (func n
    (if (eq n 0)
        1
        (mul n (fact (sub n 1)))))
  (fact 5))
Этот код определяет рекурсивную функцию fact и вычисляет факториал числа 5.

Как это работает
1. Парсер
Мы используем библиотеку FParsec для преобразования текста программы в абстрактное синтаксическое дерево (AST). Парсер разбирает выражения и создает соответствующие узлы AST.
Код:open FParsec

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


3. AST
AST — это структура данных, которая представляет программу. Пример узлов AST:


Num — число.
Var — переменная.
Let — объявление переменной.
If — условное выражение.
Func — определение функции.
Call — вызов функции.
Op — операция (например, сложение или умножение).
код:

type Expr =
    | Num of int
    | Var of string
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Func of string * Expr
    | Call of Expr * Expr
    | Op of string * Expr * Expr

4 Интерпретатор
Интерпретатор выполняет программу, обходя AST и вычисляя значения выражений. Он поддерживает окружение (среду), в котором хранятся значения переменных и функций.

Код:open System.Collections.Generic

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


Запуск программы
Чтобы запустить программу на FuncLang, выполните следующие шаги:

Установите F# и библиотеку FParsec.
Создайте F# файл и скопируйте в него реализацию парсера и интерпретатора.
Напишите программу на FuncLang и вызовите runProgram для её выполнения.
Пример использования:

fsharp

let factorialProgram = "
(let fact
  (func n
    (if (eq n 0)
        1
        (mul n (fact (sub n 1)))))
  (fact 5))
"

let result = runProgram factorialProgram
printfn "Result: %A" result
Этот код вычислит факториал числа 5 и выведет результат.

Использование генеративного ИИ
В процессе работы над этим проектом использовались инструменты генеративного ИИ (например, ChatGPT) для получения идей и генерации кода. Это помогло ускорить разработку и улучшить качество кода.

Имя | Роль в проекте
------------------|---------------------
Андриянов Эрик | волк - одиночка
