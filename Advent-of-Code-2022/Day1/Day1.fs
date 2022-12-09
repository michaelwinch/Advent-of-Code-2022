module Day1

open System.IO

type Calories = Calories of int
    with static member (+) (Calories x, Calories y) = Calories (x + y)
         static member Zero
             with get () =  Calories 0

type Elf =
    { Index: int
      Food: Calories list
      TotalCalories: Calories }
    
module Elf =
    let ofIntList index list =
        let food = list |> List.map Calories
        { Index = index
          Food = food
          TotalCalories = food |> List.sum }
        
        
let getElvesFromFile inputFile =
        inputFile
        |> File.ReadAllLines
        |> List.ofSeq
        |> List.splitAtValue ""
        |> List.except [[]]
        |> List.map (List.map int)
        |> List.mapi Elf.ofIntList

module Part1 =
    let run inputFile =
        getElvesFromFile inputFile
        |> List.maxBy (fun x -> x.TotalCalories)
        |> fun x -> x.TotalCalories
        
        
module Part2 =
    let run inputFile =
        getElvesFromFile inputFile
        |> List.sortByDescending (fun x -> x.TotalCalories)
        |> List.take 3
        |> List.sumBy (fun x -> x.TotalCalories)