module Functions
open System

let functionsList =  [("sin", sin)
                      ("cos", cos)
                      ("not", fun x -> -x)
                      ("sqrt", (sqrt))]

let getFunctionName (s, _) = s
let getFunction (_, f) : double->double = f

let rec findFunctionByName (index:int) (name:string) =
    if functionsList.Length = index then (false, getFunction functionsList[0])
    elif getFunctionName functionsList[index] = name then (true, getFunction functionsList[index])
    else findFunctionByName (index + 1) name

let findFuncByName = findFunctionByName 0

let rec findFunction (index:int) (str:string) =
    if str.Length = index || Char.IsDigit str[0] then (false, index, getFunction functionsList[0])
    elif Char.IsDigit str[index] || str[index] = '(' || str[index] = '-' then 
        let (isFunc, fn) = findFuncByName str[..(index-1)]
        (isFunc, index, fn)
    else findFunction (index + 1) str

let findFunc = findFunction 0