module List

let splitAtValue (value: 'a) (list: 'a list) : 'a list list =
    let rec loop (acc: 'a list list) (remaining: 'a list) =
        match remaining with
        | [] -> acc
        | x::tail ->
            if x = value then
                loop ([] :: acc) tail
            else
                let head = List.head acc
                let acc = (x :: head) :: (List.skip 1 acc)
                loop acc tail 
    loop [[]] list