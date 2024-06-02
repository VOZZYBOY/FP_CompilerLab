type Expr =
    | Num of int
    | Var of string
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Func of string * Expr
    | Call of Expr * Expr
    | Op of string * Expr * Expr
