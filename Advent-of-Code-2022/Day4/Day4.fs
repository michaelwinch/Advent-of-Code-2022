module Day4

open System.IO

let [<Literal>] inputFilePath = "Day4/Input.txt"

type SectionId = SectionId of int

type SectionAssignment =
    { Start: SectionId
      End: SectionId }
    
module SectionAssignment =
    let ofString (str: string) =
        let start, ``end`` = String.splitAtFirst "-" str
        { Start = SectionId (int start)
          End = SectionId (int ``end``) }


type SectionAssignmentPair = SectionAssignment * SectionAssignment

module SectionAssignmentPair =
    let ofString (str: string) : SectionAssignmentPair =
        let part1, part2 = String.splitAtFirst "," str
        part1 |> SectionAssignment.ofString,
        part2 |> SectionAssignment.ofString


let getSectionAssignmentPairsFromFile inputFile =
    inputFile
    |> File.ReadAllLines
    |> List.ofSeq
    |> List.map SectionAssignmentPair.ofString
    
module Part1 =
    module SectionAssignmentPair =
        let doAssignmentsFullyOverlap (x, y) =
            let doesAssignmentFullyOverlap x y =
                x.Start <= y.Start && x.End >= y.End
            
            doesAssignmentFullyOverlap x y || doesAssignmentFullyOverlap y x
            
            
    let run () =
        getSectionAssignmentPairsFromFile inputFilePath
        |> List.countBy SectionAssignmentPair.doAssignmentsFullyOverlap
        |> Map
        |> Map.find true
    
module Part2 =
    module SectionAssignmentPair =
        let doAssignmentsPartiallyOverlap (x, y) =
            let doesAssignmentPartiallyOverlap x y =
                let noOverlap = x.End < y.Start || x.Start > y.End
                not noOverlap
            
            doesAssignmentPartiallyOverlap x y || doesAssignmentPartiallyOverlap y x
            
            
    let run () =
        getSectionAssignmentPairsFromFile inputFilePath
        |> List.countBy SectionAssignmentPair.doAssignmentsPartiallyOverlap
        |> Map
        |> Map.find true