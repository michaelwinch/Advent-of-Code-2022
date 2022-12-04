module String

open System

let (|CaseInsensitiveEquals|_|) (x: string) (y: string) =
    if x.Equals(y, StringComparison.InvariantCultureIgnoreCase) then Some x
    else None
    
let splitAtFirst (separator: string) (str: string) =
    let idx = str.IndexOf separator
    str.Substring(0, idx),
    str.Substring(idx + separator.Length)