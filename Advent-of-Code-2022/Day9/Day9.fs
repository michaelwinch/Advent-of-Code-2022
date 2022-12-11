module Day9

open System
open System.IO
open Microsoft.FSharp.Core

let [<Literal>] inputFilePath = "Day9/Input.txt"

type Direction =
    | Up
    | Down
    | Left
    | Right
    
module Direction =
    let ofString =
        function
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | x -> failwithf "could not parse direction '%s'" x


type HeadMovement =
    { Direction: Direction
      Distance: int }
    
module HeadMovement =
    open Regex
    
    let ofString =
        function
        | Regex "^(U|D|L|R) (\d+)" groups ->
            { Direction = Direction.ofString groups[1].Value
              Distance = int groups[2].Value }
        | x -> failwithf "could not parse head movement '%s'" x


let getHeadMovements file =
    file
    |> File.ReadAllLines
    |> List.ofSeq
    |> List.map HeadMovement.ofString


type Position =
    { X: int
      Y: int }
    
module Position =
    let init = { X = 0; Y = 0 }
    
let calculateHeadPosition (position: Position) =
    function
    | Up -> { position with Y = position.Y + 1 }
    | Down -> { position with Y = position.Y - 1 }
    | Left -> { position with X = position.X - 1 }
    | Right -> { position with X = position.X + 1 }
    
let calculateTailPosition (tail: Position) (head: Position) : Position =
    let xDelta = head.X - tail.X
    let yDelta = head.Y - tail.Y
    
    let abs (n: int) = Math.Abs n
    let clamp n = Math.Clamp(n, -1, 1)
    
    let bothMove =
        let distances = [ abs xDelta; abs yDelta ]
        distances |> List.forall (fun n -> n >= 1)
        && distances |> List.exists (fun n -> n >= 2)
        
    let getPosition tailPosition delta =
        if bothMove || abs delta > 1 then tailPosition + clamp delta
        else tailPosition
    { X = getPosition tail.X xDelta
      Y = getPosition tail.Y yDelta }


module Part1 =
    let private getTailPositions headMovements =
        let rec loop positionsVisited headPosition tailPosition remainingMovements =
            match remainingMovements with
            | [] -> positionsVisited
            | h::t ->
                if h.Distance = 0 then
                    loop positionsVisited headPosition tailPosition t
                else
                    let headPosition = calculateHeadPosition headPosition h.Direction
                    let tailPosition = calculateTailPosition tailPosition headPosition
                    let movement = { h with Distance = h.Distance - 1 }
                    loop (tailPosition::positionsVisited) headPosition tailPosition (movement::t)
                    
        loop [] Position.init Position.init headMovements
    
    let run () =
        getHeadMovements inputFilePath
        |> getTailPositions
        |> List.distinct
        |> List.length