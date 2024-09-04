// module Parser

// type MorseToken = 
//     | Dot
//     | Dash
//     | Slash
//     | Space

// type Expr = 
//     | Assign of string * Expr
//     | Func of string * Expr
//     | Call of string
//     | Seq of Expr list
//     | While of Expr * Expr
//     | If of Expr * Expr * Expr
//     | Var of string
//     | Const of int

// let tokenize (input: string) : MorseToken list =
//     input.ToCharArray()
//     |> Array.toList
//     |> List.choose (function
//         | '.' -> Some Dot
//         | '-' -> Some Dash
//         | '/' -> Some Slash
//         | ' ' -> Some Space
//         | _ -> None)

// let rec parseExpr tokens =
//     printfn "Parsing expression: %A" tokens
//     match tokens with
//     | Dot :: Dash :: rest -> 
//         let (name, remainingTokens) = parseIdentifier rest
//         let (value, remainingTokens') = parseExpr remainingTokens
//         (Assign(name, value), remainingTokens')
//     | Dash :: Dot :: rest ->
//         let (name, remainingTokens) = parseIdentifier rest
//         let (body, remainingTokens') = parseExpr remainingTokens
//         (Func(name, body), remainingTokens')
//     | Slash :: rest -> 
//         let (name, remainingTokens) = parseIdentifier rest
//         (Call(name), remainingTokens)
//     | Dot :: Dot :: rest ->
//         let (cond, remainingTokens) = parseExpr rest
//         let (body, remainingTokens') = parseExpr remainingTokens
//         (While(cond, body), remainingTokens')
//     | Dash :: Dash :: rest ->
//         let (cond, remainingTokens) = parseExpr rest
//         let (thenExpr, remainingTokens') = parseExpr remainingTokens
//         let (elseExpr, remainingTokens'') = parseExpr remainingTokens'
//         (If(cond, thenExpr, elseExpr), remainingTokens'')
//     | Space :: rest -> parseExpr rest
//     | _ -> failwithf "Unexpected token sequence: %A" tokens

// and parseIdentifier tokens =
//     printfn "Parsing identifier: %A" tokens
//     let rec loop acc tokens =
//         match tokens with
//         | Dot :: rest -> loop (acc + "A") rest
//         | Dash :: rest -> loop (acc + "B") rest
//         | Slash :: rest -> (acc, rest)
//         | Space :: rest -> (acc, rest)
//         | [] -> (acc, [])
//         | _ -> failwithf "Unexpected token in identifier: %A" tokens
//     loop "" tokens

// let parse (tokens: MorseToken list) : Expr =
//     let rec parseSequence tokens acc =
//         printfn "Parsing sequence: %A" tokens
//         match tokens with
//         | [] -> (Seq acc, [])
//         | _ -> 
//             let (expr, remainingTokens) = parseExpr tokens
//             parseSequence remainingTokens (acc @ [expr])
//     fst (parseSequence tokens [])

module Parser

type MorseToken =
    | Dot
    | Dash
    | Space
    | Slash
    | Letter of char
    | Digit of char
    | Assign
    | Function
    | If
    | Then
    | Else
    | While
    | Do
    | End

type Expr =
    | Var of string
    | Num of int
    | Func of string * Expr list
    | Call of string * Expr list
    | Assign of string * Expr
    | If of Expr * Expr * Expr
    | While of Expr * Expr
    | Seq of Expr list

let morseToToken = function
    | ".-" -> Letter 'A'
    | "-..." -> Letter 'B'
    | "-.-." -> Letter 'C'
    | "-.." -> Letter 'D'
    | "." -> Letter 'E'
    | "..-." -> Letter 'F'
    | "--." -> Letter 'G'
    | "...." -> Letter 'H'
    | ".." -> Letter 'I'
    | ".---" -> Letter 'J'
    | "-.-" -> Letter 'K'
    | ".-.." -> Letter 'L'
    | "--" -> Letter 'M'
    | "-." -> Letter 'N'
    | "---" -> Letter 'O'
    | ".--." -> Letter 'P'
    | "--.-" -> Letter 'Q'
    | ".-." -> Letter 'R'
    | "..." -> Letter 'S'
    | "-" -> Letter 'T'
    | "..-" -> Letter 'U'
    | "...-" -> Letter 'V'
    | ".--" -> Letter 'W'
    | "-..-" -> Letter 'X'
    | "-.--" -> Letter 'Y'
    | "--.." -> Letter 'Z'
    | "-----" -> Digit '0'
    | ".----" -> Digit '1'
    | "..---" -> Digit '2'
    | "...--" -> Digit '3'
    | "....-" -> Digit '4'
    | "....." -> Digit '5'
    | "-...." -> Digit '6'
    | "--..." -> Digit '7'
    | "---.." -> Digit '8'
    | "----." -> Digit '9'
    | "/" -> Slash
    | " " -> Space
    | _ -> failwith "Invalid Morse code"

let tokenize (morseString: string) =
    let splitMorse (str: string) = str.Split(' ')
    splitMorse morseString |> Array.map morseToToken

let rec parseExpr (tokens: MorseToken array) =
    match tokens with
    | [||] -> failwith "Empty expression"
    | [| Letter l |] -> Var (string l), [||]
    | [| Digit d |] -> Num (int (string d)), [||]
    | [| Function; Letter l |] -> Func (string l, []), [||]
    | _ when tokens.[1] = Assign && Array.length tokens >= 3 ->
        match tokens with
        | [| Letter l; Assign |] ->
            let exprParsed, restTokens = parseExpr (Array.sub tokens 2 (Array.length tokens - 2))
            Assign (string l, exprParsed), restTokens
        | _ -> failwith "Invalid assignment"
    | _ when Array.length tokens > 5 && tokens.[0] = If && tokens.[2] = Then && tokens.[4] = Else && tokens.[6] = End ->
        let condParsed, rest1 = parseExpr (Array.sub tokens 1 1)
        let thenParsed, rest2 = parseExpr (Array.sub tokens 3 1)
        let elseParsed, rest3 = parseExpr (Array.sub tokens 5 1)
        If (condParsed, thenParsed, elseParsed), Array.sub tokens 7 (Array.length tokens - 7)
    | _ when Array.length tokens > 4 && tokens.[0] = While && tokens.[2] = Do && tokens.[4] = End ->
        let condParsed, rest1 = parseExpr (Array.sub tokens 1 1)
        let bodyParsed, rest2 = parseExpr (Array.sub tokens 3 1)
        While (condParsed, bodyParsed), Array.sub tokens 5 (Array.length tokens - 5)
    | _ -> failwith "Unknown expression"

let parse (tokens: MorseToken array) =
    let rec parseSeq tokens acc =
        match tokens with
        | [||] -> Seq (List.rev acc)
        | _ -> 
            let expr, rest = parseExpr tokens
            parseSeq rest (expr :: acc)
    parseSeq tokens []