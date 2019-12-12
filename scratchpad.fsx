let splitIntoGroups sequence chunkSize = 
    sequence |> Seq.chunkBySize chunkSize

let list = [1;9;10;3;2;3;11;0;99;30;40;50]

let getOpCode (codeChunk: list<int>) = 
    let validOpCodes = [ 1; 2; 99 ]
    let code = codeChunk.Head
    if not (List.contains code validOpCodes) then failwith "Unknown OpCode"
    code    

