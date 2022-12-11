module Day6

open System.IO

let [<Literal>] inputFilePath = "Day6/Input.txt"
   
let getDataFromFile inputFile =
    inputFile
    |> File.ReadAllText
    |> String.toCharArray
    |> List.ofSeq
    
let findMarker markerLength data =
    data
    |> List.windowed markerLength
    |> List.findIndex List.areItemsUnique
    |> (+) markerLength
    

module Part1 =
    let run () =
        getDataFromFile inputFilePath
        |> findMarker 4
        

module Part2 =
    let run () =
        getDataFromFile inputFilePath
        |> findMarker 14