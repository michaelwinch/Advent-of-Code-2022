module Day5

open System
open System.IO
open Regex

let [<Literal>] inputFilePath = "Day5/Input.txt"

type ColumnId = ColumnId of int
type Crate = Crate of char
module Crate =
    let unwrap (Crate x) = x

type Column =
    { Id: ColumnId
      Crates: Crate list } // head is the top
    
module Column =
    let ofChars (chars: char list) =
        let chars = chars |> List.except [' ']
        { Id = chars[0] |> Char.toInt |> ColumnId
          Crates = chars[1..] |> List.rev |> List.map Crate }
        
    let getTopCrate column =
        column.Crates |> List.tryHead
        
    let removeCrate column =
        column.Crates[0],
        { column with Crates = column.Crates[1..] }
    
    let removeCrates n column =
        column.Crates[0..n-1],
        { column with Crates = column.Crates[n..] }
        
    let addCrate column crate =
        { column with Crates = crate :: column.Crates }
        
    let addCrates column crates =
        { column with Crates = crates @ column.Crates }


let getColumn columnId columns =
    columns |> List.find (fun x -> x.Id = columnId)
    
let replaceColumn column columns =
    columns
    |> List.filter (fun x -> x.Id <> column.Id)
    |> (@) [column]

type Instruction =
    { NumberOfCrates: int
      From: ColumnId
      To: ColumnId }
    
module Instruction =
    let ofStrings (numberOfCrates: string) (from: string) (``to``: string) =
        { NumberOfCrates = numberOfCrates |> int
          From = from |> int |> ColumnId
          To = ``to`` |> int |> ColumnId }


let getColumns : string list -> Column list =
    List.map (String.toCharArray >> List.ofSeq)
    >> List.transpose
    >> List.map List.rev
    >> List.filter (List.toArray >> String >> String.startsWithNumber)
    >> List.map Column.ofChars
     
let [<Literal>] regex = """^move (\d+) from (\d) to (\d)"""
    
let getInstructions : string list -> Instruction list =
    let getInstruction =
        function
        | Regex regex groups ->
            Instruction.ofStrings
                groups[1].Value
                groups[2].Value
                groups[3].Value
            |> Some
        | _ -> None
    List.choose getInstruction

let getDataFromFile inputFile =
    inputFile
    |> File.ReadAllLines
    |> List.ofSeq
    |> List.splitAtFirst ""
    
let performProcedure movementFolder columns instructions =
    List.fold
        movementFolder
        columns
        instructions
    |> List.sortBy (fun x -> x.Id)
    
let getTopCrates columns =
    columns
    |> List.sortBy (fun x -> x.Id)
    |> List.map Column.getTopCrate
    
module Part1 =
    let private moveCrate columns from ``to`` =
        let crate, columnFrom = Column.removeCrate (getColumn from columns)
        let columnTo = Column.addCrate (getColumn ``to`` columns) crate
        columns
        |> replaceColumn columnFrom
        |> replaceColumn columnTo
    
    let private movementFolder =
        fun columns instruction ->
            let rec loop acc iterations =
                if iterations = 0 then acc
                else
                    loop (moveCrate acc instruction.From instruction.To) (iterations - 1)
            loop columns instruction.NumberOfCrates
        
    let run () =
        let columnData, instructionData = getDataFromFile inputFilePath
        (getColumns columnData, getInstructions instructionData)
        ||> performProcedure movementFolder
        |> getTopCrates
        |> List.map (Option.defaultValue (Crate ' ') >> Crate.unwrap)
        |> List.toArray
        |> String
        
        
module Part2 =
    let private movementFolder =
        fun columns instruction ->
            let crates, columnFrom =
                Column.removeCrates
                    instruction.NumberOfCrates
                    (getColumn instruction.From columns)
                    
            let columnTo =
                Column.addCrates
                    (getColumn instruction.To columns) crates
                    
            columns
            |> replaceColumn columnFrom
            |> replaceColumn columnTo
            
            
    let run () =
        let columnData, instructionData = getDataFromFile inputFilePath
        (getColumns columnData, getInstructions instructionData)
        ||> performProcedure  movementFolder
        |> getTopCrates
        |> List.map (Option.defaultValue (Crate ' ') >> Crate.unwrap)
        |> List.toArray
        |> String