module String

open System

let (|CaseInsensitiveEquals|_|) (x: string) (y: string) =
    if x.Equals(y, StringComparison.InvariantCultureIgnoreCase) then Some x
    else None