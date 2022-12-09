module Day6

open System.IO

   
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
    let run inputFile =
        getDataFromFile inputFile
        |> findMarker 4
        

module Part2 =
    let run inputFile =
        getDataFromFile inputFile
        |> findMarker 14