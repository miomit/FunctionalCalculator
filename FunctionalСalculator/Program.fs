open Lexer
open Tree

let rec cal tree = 
    match tree with
    | Leaf num -> num
    | UnoBranch (fn, child) -> fn (cal child)
    | BinBranch (fn, childLeft, childRight) -> fn (cal childLeft) (cal childRight)

"2+2*2"
|> lexer
|> cal
|> printfn "%f"