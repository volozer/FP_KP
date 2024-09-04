module Program

open Parser
open Interpreter

let run code =
    try
        let tokens = tokenize code
        let ast = parse tokens
        let result = eval [] ast
        printfn "Result: %A" result
    with
    | ex -> printfn "Error: %s" (ex.Message)

[<EntryPoint>]
let main argv =
    // Примеры кода на MorseLang для тестирования
    let code1 = ".- ... .. -- -. / -... / .... . .-.. .-.. --- / .-- --- .-. .-.. -.."  // пример присваивания и вызова функции
    let code2 = ".-- .... .. .-.. .-.. / -.-. --- -. -.. / -.. --- / -... --- -.. -.-- / . -. -.."  // пример цикла while
    let code3 = ".. ..-. / -.-. --- -. -.. / - .... . -. / - .... . -. .- -.- / . .-.. ... . / . -. -.."  // пример условия if

    run code1
    run code2
    run code3

    0