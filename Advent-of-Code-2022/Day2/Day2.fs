module Day2

open System.IO

let [<Literal>] inputFilePath = "Day2/Input.txt"

type EncryptedValue = EncryptedValue of string

type Hand =
    | Rock
    | Paper
    | Scissors
    
module Hand =
    let decrypt =
        function
        | EncryptedValue (String.CaseInsensitiveEquals "A" _) -> Rock
        | EncryptedValue (String.CaseInsensitiveEquals "B" _) -> Paper
        | EncryptedValue (String.CaseInsensitiveEquals "C" _) -> Scissors
        | _ -> failwith "invalid hand"

    let getScore =
        function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
        
    let getWinningResponse =
        function
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock
        
    let getLosingResponse =
        function
        | Rock -> Scissors
        | Scissors -> Paper
        | Paper -> Rock
        
    let getDrawingResponse (x: Hand) = x


type ThrowResult =
    | Win
    | Lose
    | Draw
    
module ThrowResult =
    let getScore =
        function
        | Win -> 6
        | Lose -> 0
        | Draw -> 3
        
    let calculate (opponent: Hand) (response: Hand) =
        match opponent, response with
        | Rock, Rock
        | Paper, Paper
        | Scissors, Scissors -> Draw
        | Rock, Paper
        | Paper, Scissors
        | Scissors, Rock -> Win
        | Rock, Scissors
        | Scissors, Paper
        | Paper, Rock -> Lose


type Round =
    { Opponent: Hand
      Response: Hand
      Score: int }
    
module Round =
    let calculateScore (opponent: Hand) (response: Hand) =
        let throwScore =
            ThrowResult.calculate opponent response
            |> ThrowResult.getScore
        let handScore = Hand.getScore response
        throwScore + handScore

        
let getEncryptedValuesFromFile inputFile =
    inputFile
    |> File.ReadAllLines
    |> List.ofSeq
    |> List.map (fun x -> x.Split " " |> fun x -> EncryptedValue x[0], EncryptedValue x[1])

module Part1 =
    let private decryptResponse =
        function
        | EncryptedValue (String.CaseInsensitiveEquals "X" _) -> Rock
        | EncryptedValue (String.CaseInsensitiveEquals "Y" _) -> Paper
        | EncryptedValue (String.CaseInsensitiveEquals "Z" _) -> Scissors
        | _ -> failwith "invalid throw result"
        
    let private roundOfEncryptedValues (encryptedOpponent: EncryptedValue, encryptedResponse: EncryptedValue) =
        let opponent = Hand.decrypt encryptedOpponent
        let response = decryptResponse encryptedResponse
        { Opponent = opponent
          Response = response
          Score = Round.calculateScore opponent response }
        
    let run () =
        getEncryptedValuesFromFile inputFilePath
        |> List.map roundOfEncryptedValues
        |> List.sumBy (fun x -> x.Score)
        
        
module Part2 =
    let private decryptThrowResult =
        function
        | EncryptedValue (String.CaseInsensitiveEquals "X" _) -> Lose
        | EncryptedValue (String.CaseInsensitiveEquals "Y" _) -> Draw
        | EncryptedValue (String.CaseInsensitiveEquals "Z" _) -> Win
        | _ -> failwith "invalid throw result"
        
    let getResponseForResult (opponent: Hand) =
        function
        | Win -> Hand.getWinningResponse opponent
        | Lose -> Hand.getLosingResponse opponent
        | Draw -> Hand.getDrawingResponse opponent
    
    let private roundOfEncryptedValues (encryptedOpponent: EncryptedValue, encryptedThrowResult: EncryptedValue) =
        let opponent = Hand.decrypt encryptedOpponent
        let throwResult = decryptThrowResult encryptedThrowResult
        let response = getResponseForResult opponent throwResult
        { Opponent = opponent
          Response = response
          Score = Round.calculateScore opponent response }
    
    let run () =
        getEncryptedValuesFromFile inputFilePath
        |> List.map roundOfEncryptedValues
        |> List.sumBy (fun x -> x.Score)