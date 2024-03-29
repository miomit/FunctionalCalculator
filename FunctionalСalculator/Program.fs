﻿open Lexer
open Tree

let rec cal tree = 
    match tree with
    | Leaf num -> num
    | UnoBranch (fn, child) -> fn (cal child)
    | BinBranch (fn, childLeft, childRight) -> fn (cal childLeft) (cal childRight)

"18*(1/9)^2*sin(60)-20*(1/9)"
|> lexer
|> cal
|> printfn "%f"