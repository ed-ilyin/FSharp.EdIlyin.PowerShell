module FSharp.EdIlyin.PowerShell

open System.Management.Automation
open EdIlyin.FSharp.Elm.Core
// open EdIlyin.FSharp.Elm.Core.Decode


let property name decoder =
    let label =
        sprintf "%s property '%s'" (Decode.getLabel decoder) name

    Decode.primitive label
        (fun (psobj: obj) ->
            try
                match psobj with
                    | :? PSObject as pso ->
                        let prop = pso.Properties.Item name
                        Decode.run decoder prop.Value

                    | typ ->
                        label
                            => sprintf ", but got unexpected %A %A"
                                (typ.GetType ())
                                psobj
                            |> Err

            with | error ->
                label => sprintf "in %A, but got unexpected %A" psobj error |> Err
        )


let primitive<'T> label =
    Decode.primitive label
        (fun (input: obj) ->
            match input with
                | :? 'T as res -> Ok res

                | typ ->
                    label
                        => sprintf ", but got unexpected %A %A"
                            (typ.GetType ())
                            input
                        |> Err
        )


let string = primitive<string> "string"


let int = primitive<int> "int"


let int64 = primitive<int> "int64" |> Decode.map int64


let bool = primitive<bool> "bool"


let array decoder =
    let label = Decode.getLabel decoder |> sprintf "%s Array"
    primitive<obj []> label
        |> Decode.andThen
            (Array.map (Decode.decodeValue decoder)
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
                            Decode.decodeValue decoder x.Value
                                |> Result.map ((=>) x.Name)
                        )
                    |> Result.combine
                    |> Result.map Map.ofSeq
                    |> Decode.fromResult
            )


let at path decoder = List.foldBack property path decoder
