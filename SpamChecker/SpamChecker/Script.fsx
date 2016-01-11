type DocType =
    | Ham
    | Spam

let identify (example:DocType*string) = 
    let docType,content = example
    match docType with
    | Ham -> printfn "'%s' is ham" content
    | Spam -> printfn "'%s' is spam" content

