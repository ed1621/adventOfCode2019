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

    let rec programAlarm (groups: list<int list>, codeSeq: list<int>) = 
        let workingGroup = groups.Head
        let opCode = getOpCode workingGroup
        if (opCode = 1) then
            let calculationResult = codeSeq.[workingGroup.[1]] + codeSeq.[workingGroup.[2]]
            let newCodeSeq = updateCodeSequence workingGroup calculationResult codeSeq
            programAlarm (groups.[1..], newCodeSeq)
        elif (opCode = 2) then 
            let calculationResult = codeSeq.[workingGroup.[1]] * codeSeq.[workingGroup.[2]]
            let newCodeSeq = updateCodeSequence workingGroup calculationResult codeSeq
            programAlarm (groups.[1..], newCodeSeq)
        elif (opCode = 99) then codeSeq    
        else failwith "Unknown opcode"    

    let setIntCodeParameters (noun, verb, intCode) =
        updateElement 1 noun intCode
        |> updateElement 2 verb

    let runProgram (noun: int, verb: int) = 
        let originalList = [1;2;3;3;1;1;2;3;1;3;4;3;1;5;0;3;2;6;1;19;1;5;19;23;2;9;23;27;1;6;27;31;1;31;9;35;2;35;10;39;1;5;39;43;2;43;9;47;1;5;47;51;1;51;5;55;1;55;9;59;2;59;13;63;1;63;9;67;1;9;67;71;2;71;10;75;1;75;6;79;2;10;79;83;1;5;83;87;2;87;10;91;1;91;5;95;1;6;95;99;2;99;13;103;1;103;6;107;1;107;5;111;2;6;111;115;1;115;13;119;1;119;2;123;1;5;123;0;99;2;0;14;0] 
        let newintCode = setIntCodeParameters (noun, verb, originalList)
        let groups = splitIntoGroups newintCode 4
        programAlarm (groups, newintCode)

