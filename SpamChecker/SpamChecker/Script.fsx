open System.IO
#load "NaiveBayes.fs"
open NaiveBayes.Classifier
open System.Text.RegularExpressions

let matchWords = Regex(@"\w+")
let wordTokenizer (text:string) = 
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

type DocType =
    | Ham
    | Spam


let parseDocType (label:string) =
    match label with
    | "ham" -> Ham
    | "spam" -> Spam
    | _ -> failwith "Unknown label"

let parseLine (line:string) = 
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let identify (example:DocType*string) = 
    let docType,content = example
    match docType with
    | Ham -> printfn "'%s' is ham" content
    | Spam -> printfn "'%s' is spam" content

let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"..\..\Data\" + fileName

let dataset = 
    File.ReadAllLines path
    |> Array.map parseLine


let validation, training = dataset.[..999], dataset.[1000..]

let txtClassifier = train training wordTokenizer (["txt"] |> set)

validation 
    |> Seq.averageBy (fun (docType, sms) ->
        if docType = txtClassifier sms then 1.0 else 0.0)
    |> printfn "Based on 'txt', correctly classified: %.3f"

