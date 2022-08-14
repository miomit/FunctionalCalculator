module Operations

let operationsList = [('-', (-))
                      ('+', (+))
                      ('/', (/))
                      ('*', (*))
                      ('^', (fun x y -> x**y))]

let getOperationSymvol (s, _) = s
let getOperationFunction (_, f) : double->double->double = f

let rec findByValue (index:int) (bracketsCount) (str:string) (value:char) =
    if str.Length = index then -1
    elif str[index] = '(' then findByValue (index + 1) (bracketsCount + 1) str value
    elif str[index] = ')' then findByValue (index + 1) (bracketsCount - 1) str value
    elif bracketsCount > 0 then findByValue (index + 1) bracketsCount str value
    elif str[index] = value then index
    else findByValue (index + 1) bracketsCount str value

let findByVal = findByValue 0 0

let rec findOperation (index:int)(str:string) =
    if operationsList.Length = index then (false, -1, getOperationFunction operationsList[0]) else
    
    let symbol = getOperationSymvol operationsList[index]

    let id = findByVal str symbol
    
    if id > -1 then (true, id, getOperationFunction operationsList[index])
    else findOperation (index + 1) str

let findOpr = findOperation 0