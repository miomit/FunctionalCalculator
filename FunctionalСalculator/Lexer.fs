module Lexer
open Operations
open Tree

let rec lexer (line:string) : Tree =
    let (isOpr, id, fn) = findOpr line
    if isOpr then 
        printfn $"{line[..id - 1]} - {line[id]} - {line[id + 1..]}"
        BinBranch(fn, lexer(line[..id - 1]), lexer(line[id + 1..]))
    else Leaf (double line)