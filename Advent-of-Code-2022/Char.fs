module Char

open System

let isNumber (char: char) =
    string char
    |> Int32.TryParse
    |> fst
    
let toInt (char: char) =
    string char
    |> Int32.Parse