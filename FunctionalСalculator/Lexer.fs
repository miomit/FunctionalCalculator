module Lexer
open Operations
open Functions
open Tree

let rec finFirsCloseBracket (index:int) (bracketsCount) (str:string) =
    if str.Length = index then -1
    elif str[index] = '(' then finFirsCloseBracket (index + 1) (bracketsCount + 1) str
    elif str[index] = ')' && bracketsCount > 0 then finFirsCloseBracket (index + 1) (bracketsCount - 1) str
    elif bracketsCount > 0 then finFirsCloseBracket (index + 1) bracketsCount str
    elif str[index] = ')' then index
    else finFirsCloseBracket (index + 1) bracketsCount str

let closeBracketId = finFirsCloseBracket 0 0

let openingBracket (line:string) =
    line[1..(closeBracketId line[1..])]

let rec lexer (line:string) : Tree =
    let (isOpr, id, fn) = findOpr line
    if isOpr then BinBranch(fn, lexer(line[..id - 1]), lexer(line[id + 1..]))
    elif line[0] = '(' then lexer(openingBracket line) else

    let (isFunc, length, fn) = findFunc line
    if isFunc then UnoBranch(fn, lexer(line[length..]))
    else Leaf (double line)