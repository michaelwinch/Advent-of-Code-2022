[<AutoOpen>]
module Composition

let (|>>) f1 f2 x = f1 x ||> f2