module Day7

open System.IO
open Regex


[<AutoOpen>]
module Domain =
    type Directory =
        { Name: string
          Contents: Item list
          Size: int option }
    
    and File =
        { Name: string
          Size: int }
    
    and Item =
        | Directory of Directory
        | File of File
        
    let root : Directory =
        { Name = "/"
          Contents = []
          Size = None }
        
    module Item =
        let getSize =
            function
            | Directory dir -> dir.Size
            | File f -> Some f.Size
        
    module Directory =
        let findDirectory name (dir: Directory) : Directory =
            dir.Contents
            |> List.choose
                   (function
                    | Directory x ->
                        if x.Name = name then Some x
                        else None
                    | File _ -> None)
            |> List.head
            
        let updateDirectory (parent: Directory) (child: Directory) : Directory =
            { parent with
                Contents =
                    parent.Contents
                    |> List.map
                           (function
                            | Directory dir when dir.Name = child.Name ->
                                Directory { dir with Contents = child.Contents }
                            | x -> x) }            


type Command =
    | CD of directoryName: string
    | CDUpDir
    | LS of results: string list
    
module Command =
    open Domain
    
    let getItemsFromLSResults (results: string list) =
        let parseItem =
            function
            | Regex "^dir (\w+)" groups ->
                Directory { Name = groups[1].Value; Contents = []; Size = None }
            | Regex "^(\d+) (.*)$" groups ->
                File { Name = groups[2].Value; Size = int groups[1].Value }
            | x -> failwithf "could not parse ls result '%s'" x 
        results |> List.map parseItem


let parseCommands (input: string list) : Command list =
    let rec loop acc remaining =
        match remaining with
        | [] -> acc
        | h::t ->
            match h with
            | Regex "^\$ cd \.\." _ ->
                loop (CDUpDir :: acc) t
            | Regex "^\$ cd (.+)" groups ->
                loop (CD groups[1].Value :: acc) t
            | Regex "^\$ ls" _ ->
                let index = t |> List.tryFindIndex (fun x -> x.StartsWith "$")
                match index with
                | Some idx ->
                    let results, rest = t |> List.splitAt idx
                    loop (LS results :: acc) rest
                | None -> // end of file
                    LS t :: acc
            | x -> failwithf "could not parse ls result '%s'" x
    loop [] input
    |> List.rev
    
let buildFileTree (commands: Command list) : Directory =
    let removeInitialCDRoot (commands: Command list) =
        if commands[0] = CD "/" then commands[1..] else commands
    
    let rec buildTree (acc: Directory) remainingCommands =
        match remainingCommands with
        | [] -> acc, []
        | h::t ->
            match h with
            | CD dir ->
                let dir, unusedCommands = buildTree (acc |> Directory.findDirectory dir) t
                let acc = Directory.updateDirectory acc dir
                buildTree acc unusedCommands
            | CDUpDir ->
                acc, t
            | LS results ->
                let acc = { acc with Contents = Command.getItemsFromLSResults results }
                buildTree acc t
                
    let rec calculateDirectorySizes (dir: Directory) =
        let dir =
            { dir with
                Contents =
                    dir.Contents
                    |> List.map
                           (function
                            | Directory d when d.Size = None -> Directory (calculateDirectorySizes d)
                            | x -> x)
            }
    
        { dir with
            Size =
                dir.Contents |> List.choose Item.getSize |> List.sum |> Some }
    
    commands
    |> removeInitialCDRoot
    |> buildTree Domain.root
    |> fst
    |> calculateDirectorySizes

let getFileTree file =
    file
    |> File.ReadAllLines
    |> List.ofSeq
    |> parseCommands
    |> buildFileTree


module Part1 =
    let private sumDirectorySizesWithMaxValue maxValue dir =
        let rec loop dir =
            let childDirs =
                dir.Contents
                |> List.choose (function Directory d -> Some d | _ -> None)
                
            let sizes =
                childDirs
                |> List.choose (fun x -> x.Size)
                |> List.filter (fun x -> x <= maxValue)
            let childSizes = childDirs |> List.collect loop
            sizes @ childSizes
            
        loop dir
        |> List.sum
    
    let run inputFile =
        getFileTree inputFile
        |> sumDirectorySizesWithMaxValue 100000
        
        
module Part2 =
    
    let [<Literal>] diskSpace = 70000000
    let [<Literal>] spaceRequired = 30000000
    
    let private findSmallestDirectorySizeOverMinValue minValue dir =
        let rec loop dir =
            let childDirs =
                dir.Contents
                |> List.choose (function Directory d -> Some d | _ -> None)
                
            let sizes =
                childDirs
                |> List.choose (fun x -> x.Size)
                |> List.filter (fun x -> x >= minValue)
            let childSizes = childDirs |> List.collect loop
            sizes @ childSizes
            
        loop dir
        |> List.min
    
    let run inputFile =
        let fileTree = getFileTree inputFile
        let unusedSpace = diskSpace - fileTree.Size.Value
        let spaceToFreeUp = spaceRequired - unusedSpace
        findSmallestDirectorySizeOverMinValue spaceToFreeUp fileTree