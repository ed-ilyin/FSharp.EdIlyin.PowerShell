module FSharp.EdIlyin.PowerShell

open System.Management.Automation
open FSharp.EdIlyin.Core


let field name decoder =
    let label =
        Decode.getLabel decoder |> sprintf "%s -> %s" name

    Decode.primitive label
        (fun (psobj: obj) ->
            try
                match psobj with
                    | :? PSObject as pso ->
                        let prop = pso.Properties.Item name
                        Decode.run decoder prop.Value

                    | typ -> Decode.expectingButGot label psobj

            with | error -> Decode.expectingButGot label psobj
        )


let property = field


let primitive<'T> label =
    Decode.primitive label
        (fun (input: obj) ->
            match input with
                | :? 'T as res -> Ok res
                | x -> x.GetType () |> Decode.expectingButGot label
        )


let string = primitive<string> "an String"


let int = primitive<int> "an Int"


let int64 = primitive<int64> "an Int64"


let uint32 = primitive<uint32> "an UInt32"


let bool = primitive<bool> "a Boolean"


let array decoder =
    let label = Decode.getLabel decoder |> sprintf "%s Array"
    primitive<obj []> label
        |> Decode.andThen
            (Array.map (Decode.decode decoder)
                >> Result.combineArray
                >> Decode.fromResult
            )


let dict decoder =
    let label = Decode.getLabel decoder |> sprintf "maping of string to %s"
    primitive<PSObject> label
        |> Decode.andThen
            (fun pso ->
                pso.Properties
                    |> Seq.map
                        (fun x ->
                            Decode.decode decoder x.Value
                                |> Result.map ((=>) x.Name)
                        )
                    |> Result.combine
                    |> Result.map Map.ofSeq
                    |> Decode.fromResult
            )


let at path decoder = List.foldBack property path decoder


let list decoder =
    let label = Decode.getLabel decoder |> sprintf "%s List"
    primitive<obj []> label
        |> Decode.andThen
            (List.ofArray
                >> List.map (Decode.decode decoder)
                >> Result.combineList
                >> Decode.fromResult
            )
