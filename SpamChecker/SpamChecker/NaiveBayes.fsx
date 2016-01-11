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

let casedTokenizer (text:string) = 
    text
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

let casedTokens =
    training
    |> Seq.map snd
    |> vocabulary casedTokenizer

let evaluate (tokenizer:Tokenizer) (tokens:Token Set) = 
        let classifier = train training tokenizer tokens
        validation
        |> Seq.averageBy (fun (docType, sms) ->
            if docType = classifier sms then 1.0 else 0.0)
        |> printfn "Correctly classified: %.3f"


let ham, spam = 
    let rawHam, rawSpam =
        training
        |> Array.partition (fun (lbl,_) -> lbl=Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd

let hamCount = ham |> vocabulary casedTokenizer |> Set.count
let spamCount = spam |> vocabulary casedTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> top (spamCount / 10) casedTokenizer
let topTokens = Set.union topHam topSpam

let txtClassifier = train training wordTokenizer (["txt"] |> set)
let fullClassifier = train training wordTokenizer allTokens

ham |> top 20 casedTokenizer |> Seq.iter (printfn "%s")
spam |> top 20 casedTokenizer |> Seq.iter (printfn "%s")

let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens


validation 
    |> Seq.averageBy (fun (docType, sms) ->
        if docType = fullClassifier sms then 1.0 else 0.0)
    |> printfn "Based on 'txt', correctly classified: %.3f"

printfn "word: all tokens"
evaluate wordTokenizer allTokens;;

printfn "cased: cased tokens"
evaluate casedTokenizer casedTokens;;

printfn "cased: top tokens"
evaluate casedTokenizer topTokens;;

printfn "cased: specific tokens"
evaluate casedTokenizer specificTokens