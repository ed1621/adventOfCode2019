namespace adventOfCode.dayOne

open System.IO

module tyrannyOfTheRocketEquation = 

    let divideByThree input  = 
        input / 3

    let roundDown input = 
        float input
        |> floor

    let subtractTwo input =
        let result = input - 2.0
        int result

    let rec calculateFuelForSingleModule mass = 
        let fuelNeeded = divideByThree mass |> roundDown |> subtractTwo
        if fuelNeeded >= 0 then 
            fuelNeeded + calculateFuelForSingleModule fuelNeeded
        else 0    

    let calculateTotalFuelRequirement modulesFilePath = 
        let listOfModules = 
            File.ReadLines(modulesFilePath)
        listOfModules |> Seq.sumBy (int >> calculateFuelForSingleModule)
