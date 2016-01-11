open System.IO

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


let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"..\..\Data\" + fileName

let dataset = 
    File.ReadAllLines path
    |> Array.map parseLine


open System.Text.RegularExpressions
let matchWords = Regex(@"\w+")
let wordTokenizer (text:string) = 
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq



#load "NaiveBayes.fs"
open NaiveBayes.Classifier

let validation, training = dataset.[..999], dataset.[1000..]

let allTokens = 
    training
    |> Seq.map snd 
    |> vocabulary wordTokenizer


let evaluate (tokenizer:Tokenizer) (tokens:Token Set) = 
        let classifier = train training tokenizer tokens
        validation
        |> Seq.averageBy (fun (docType, sms) ->
            if docType = classifier sms then 1.0 else 0.0)
        |> printfn "Correctly classified: %.3f"

let txtClassifier = train training wordTokenizer (["txt"] |> set)
let fullClassifier = train training wordTokenizer allTokens

validation 
    |> Seq.averageBy (fun (docType, sms) ->
        if docType = fullClassifier sms then 1.0 else 0.0)
    |> printfn "Based on 'txt', correctly classified: %.3f"

evaluate wordTokenizer allTokens;;