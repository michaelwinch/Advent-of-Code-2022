module Day8

open System.IO
open Microsoft.FSharp.Core

let [<Literal>] inputFilePath = "Day8/Input.txt"

type Tree =
    { XPosition: int
      YPosition: int
      Height: int }
    
type TreeData =
    { XLength: int
      YLength: int
      Trees: Tree list }

module TreeData =
    let getTreesNorthOfPos data (x, y) =
        data.Trees
        |> List.filter (fun t -> t.YPosition = y)
        |> List.filter (fun t -> t.XPosition < x)
        |> List.sortByDescending (fun t -> t.XPosition)
        
    let getTreesSouthOfPos data (x, y) =
        data.Trees
        |> List.filter (fun t -> t.YPosition = y)
        |> List.filter (fun t -> t.XPosition > x)
        |> List.sortBy (fun t -> t.XPosition)
        
    let getTreesEastOfPos data (x, y) =
        data.Trees
        |> List.filter (fun t -> t.XPosition = x)
        |> List.filter (fun t -> t.YPosition > y)
        |> List.sortBy (fun t -> t.YPosition)
        
    let getTreesWestOfPos data (x, y) =
        data.Trees
        |> List.filter (fun t -> t.XPosition = x)
        |> List.filter (fun t -> t.YPosition < y)
        |> List.sortByDescending (fun t -> t.YPosition)


let getTreeData file =
    let lines =
        file
        |> File.ReadAllLines
    
    let trees =
        lines
        |> Array.mapi
               (fun y line ->
                    String.toCharArray line
                    |> Array.mapi
                           (fun x height ->
                                { XPosition = x
                                  YPosition = y
                                  Height = Char.toInt height }))
        |> Array.collect id
        |> List.ofArray
        
    { XLength = lines[0] |> String.length
      YLength = lines |> Array.length
      Trees = trees }
    
let getTree trees (x,y) =
    trees
    |> List.find (fun t -> t.XPosition = x && t.YPosition = y)

let isTreeOnAnEdge data tree =
    tree.XPosition = data.XLength - 1
    || tree.YPosition = data.YLength - 1

module Part1 =
    let private isTreeVisibleInLine height trees =
        if trees |> List.isEmpty then true else
            trees
            |> List.map (fun t -> t.Height)
            |> List.max
            |> (>) height
        
    let private isTreeVisible data tree =
        if isTreeOnAnEdge data tree then
            true
        else
            let pos = tree.XPosition, tree.YPosition
            isTreeVisibleInLine tree.Height (TreeData.getTreesNorthOfPos data pos)
            || isTreeVisibleInLine tree.Height (TreeData.getTreesSouthOfPos data pos)
            || isTreeVisibleInLine tree.Height (TreeData.getTreesEastOfPos data pos)
            || isTreeVisibleInLine tree.Height (TreeData.getTreesWestOfPos data pos)

    let run () =
        let data = getTreeData inputFilePath
        data.Trees
        |> List.filter (isTreeVisible data)
        |> List.length
        
        
module Part2 =
    type ViewingDistance =
        { North: int
          South: int
          East: int
          West: int }
        
    module ViewingDistance =
        let calculateScenicScore vd =
            vd.North * vd.South * vd.East * vd.West
            
        let rec private calculateViewingDistanceInLine trees tree =
            let rec loop acc remaining =
                match remaining with
                | [] -> acc
                | h::t ->
                    if h.Height >= tree.Height then
                        acc + 1
                    else
                        loop (acc + 1) t
                        
            loop 0 trees
            
        let calculate data tree : ViewingDistance =
            let pos = tree.XPosition, tree.YPosition
            { North = calculateViewingDistanceInLine (TreeData.getTreesNorthOfPos data pos) tree
              South = calculateViewingDistanceInLine (TreeData.getTreesSouthOfPos data pos) tree
              East = calculateViewingDistanceInLine (TreeData.getTreesEastOfPos data pos) tree
              West = calculateViewingDistanceInLine (TreeData.getTreesWestOfPos data pos) tree }
            
    
    let run () =
        let data = getTreeData inputFilePath
        data.Trees
        |> List.map (ViewingDistance.calculate data >> ViewingDistance.calculateScenicScore)
        |> List.max
        