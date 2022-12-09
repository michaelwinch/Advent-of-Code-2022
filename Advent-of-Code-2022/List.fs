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

let splitAtFirst (separator: 'a) (list: 'a list) =
    let index = List.findIndex (fun x -> x = separator) list
    List.splitAt index list
    
let areItemsUnique list =
    list = List.distinct list
