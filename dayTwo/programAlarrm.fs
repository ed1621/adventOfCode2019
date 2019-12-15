namespace adventOfCode.dayTwo

module ProgramAlarmChallenge = 
    let splitIntoGroups sequence chunkSize = 
        sequence |> List.chunkBySize chunkSize |> Seq.toList

    let getOpCode (codeChunk: list<int>) = 
        let validOpCodes = [ 1; 2; 99 ]
        let code = codeChunk.Head
        if not (List.contains code validOpCodes) then failwith "Unknown OpCode"
        code    
        
    let updateElement indexToUpdate updatedValue list = 
        list |> List.mapi (fun listIndex value -> if listIndex = indexToUpdate then updatedValue else value)  

    let updateCodeSequence sequence value sequenceToUpdate =
        let indexToChange = Seq.last sequence
        updateElement indexToChange value sequenceToUpdate

    let rec programAlarm (codeSeq: list<int>, groups: list<int list>) = 
        let workingGroup = groups.Head
        let opCode = getOpCode workingGroup
        if (opCode = 1) then
            let calculationResult = codeSeq.[workingGroup.[1]] + codeSeq.[workingGroup.[2]]
            let newCodeSeq = updateCodeSequence workingGroup calculationResult codeSeq
            programAlarm (newCodeSeq, groups.[1..])
        elif (opCode = 2) then 
            let calculationResult = codeSeq.[workingGroup.[1]] * codeSeq.[workingGroup.[2]]
            let newCodeSeq = updateCodeSequence workingGroup calculationResult codeSeq
            programAlarm (newCodeSeq, groups.[1..])
        elif (opCode = 99) then codeSeq    
        else failwith "Unknown opcode"    
