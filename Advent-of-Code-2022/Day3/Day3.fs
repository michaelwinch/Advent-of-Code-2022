module Day3

type ItemType = ItemType of char

module ItemType =
    let private itemTypePriorityMap =
        [ [ 'a'..'z' ]
          [ 'A'..'Z' ] ]
        |> List.collect id
        |> List.mapi (fun index value -> value, index + 1)
        |> Map
        
    let getPriority (ItemType itemType) : int =
        itemTypePriorityMap |> Map.find itemType


type Item = Item of ItemType

module Item =
    let getItemType (Item itemType) = itemType
    

type Rucksack =
    { Items: Item list }
    
module Rucksack =
    let ofString (str: string) =
        { Items =
            str.ToCharArray()
            |> List.ofSeq
            |> List.map (ItemType >> Item) }

        
let getRucksacksFromFile inputFile =
    inputFile
    |> File.readAllLines
    |> List.ofSeq
    |> List.map Rucksack.ofString

// Better solution is to use Set.intersect
let getIntersectingItemTypes itemTypes1 itemTypes2 : ItemType list =
    itemTypes1
    |> List.filter (fun x -> itemTypes2 |> List.exists (fun y -> x = y))
    |> List.distinct

module Part1 =
    type CompartmentalisedRucksack =
        { Compartment1: Item list
          Compartment2: Item list }
        
    module CompartmentalisedRucksack =
        let ofRucksack rucksack =
            let compartment1, compartment2 =
                rucksack.Items
                |> fun x -> List.splitAt (x.Length / 2) x
            
            { Compartment1 = compartment1
              Compartment2 = compartment2 }
            
        let getItemsTypes rucksack =
            rucksack.Compartment1 |> List.map Item.getItemType,
            rucksack.Compartment2 |> List.map Item.getItemType
            
            
    let run inputFile =
        getRucksacksFromFile inputFile
        |> List.map CompartmentalisedRucksack.ofRucksack
        |> List.collect (CompartmentalisedRucksack.getItemsTypes |>> getIntersectingItemTypes)
        |> List.sumBy ItemType.getPriority
        

module Part2 =
    module Rucksack =
        let getItemTypes rucksack =
            rucksack.Items
            |> List.map Item.getItemType
    
    type Group = Group of Rucksack list
    
    module Group =
        let findCommonItemType (Group rucksacks) : ItemType =
            let rucksacksItemTypes = rucksacks |> List.map Rucksack.getItemTypes
            
            List.fold
                (fun itemTypesInAllSoFar itemTypesInCurrentRucksack ->
                    itemTypesInAllSoFar
                    |> List.filter (fun x -> itemTypesInCurrentRucksack |> List.contains x)
                )
                rucksacksItemTypes[0]
                rucksacksItemTypes
            |> List.head
    
    
    let private splitIntoGroups groupSize =
        List.chunkBySize groupSize
        >> List.map Group
    
    let run inputFile =
        getRucksacksFromFile inputFile
        |> splitIntoGroups 3
        |> List.map Group.findCommonItemType
        |> List.sumBy ItemType.getPriority