module Interpreter

open Parser

let rec eval (env: (string * int) list) (expr: Expr) : (string * int) list =
    printfn "Evaluating expression: %A" expr
    match expr with
    | Assign (name, value) ->
        let evaluatedValue = evalExpr env value
        printfn "Assigning: %s = %d" name evaluatedValue
        (name, evaluatedValue) :: env
    | Func (name, body) -> env
    | Call (name, _) ->
        match List.tryFind (fun (n, _) -> n = name) env with
        | Some (_, value) -> 
            printfn "Calling function: %s, value: %d" name value
            env
        | None -> failwithf "Function '%s' not found" name
    | Seq exprs -> List.fold eval env exprs
    | While (cond, body) ->
        let rec loop env =
            if evalExpr env cond <> 0 then
                let env' = eval env body
                loop env'
            else
                env
        loop env
    | If (cond, thenExpr, elseExpr) ->
        if evalExpr env cond <> 0 then eval env thenExpr
        else eval env elseExpr
    | Var name -> env
    | Num value -> env

and evalExpr (env: (string * int) list) (expr: Expr) : int =
    printfn "Evaluating expression: %A" expr
    match expr with
    | Var name ->
        match List.tryFind (fun (n, _) -> n = name) env with
        | Some (_, value) -> value
        | None -> failwithf "Variable '%s' not found" name
    | Num value -> value
    | _ -> failwith "Unexpected expression"

// module Interpreter

// open Parser

// let rec eval env = function
//     | Var v -> 
//         match List.tryFind (fun (name, _) -> name = v) env with
//         | Some (_, value) -> value
//         | None -> failwithf "Undefined variable: %s" v
//     | Num n -> n
//     | Func (name, args) -> fun values -> 
//         let newEnv = List.zip args values @ env
//         eval newEnv name
//     | Call (name, args) -> 
//         let func = eval env (Var name)
//         let argValues = List.map (eval env) args
//         func argValues
//     | Assign (name, expr) -> 
//         let value = eval env expr
//         (name, value) :: env
//     | If (cond, thenExpr, elseExpr) ->
//         if eval env cond <> 0 then eval env thenExpr else eval env elseExpr
//     | While (cond, body) ->
//         let rec loop env =
//             if eval env cond <> 0 then
//                 let newEnv = eval env body
//                 loop newEnv
//             else env
//         loop env
//     | Seq exprs ->
//         List.fold (fun acc expr -> eval acc expr) env exprs