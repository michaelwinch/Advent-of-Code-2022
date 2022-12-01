module File

open System.IO

let readAllLines (filePath: string) =
    File.ReadAllLines filePath