
#r "packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#load "Common.fs"
#load "Types.fs"
#load "IO.fs"

open SongsBackup.Common
open SongsBackup.Types
open SongsBackup.IO
   

let flow = result {
    let! jsonPath = Paths.createValidJsonPath @"D:\personal\projects\SongsBackup\data.json"
                   |> Result.mapError Errors.validationToProcessError
    let! info = Reading.loadFromJson jsonPath
                |> Result.mapError Errors.fileToProcessError
    let results = Paths.createValidPath @"\\NICKLE_NAS"
                   |> Option.map (Search.getFiles info)
                   |> Option.flatten   
   
               
    return results
}

printfn "%A" flow


//let test = Directory.GetFiles(@"\\NICKLE_NASx\music", @"06 - Don't say xx ''lazy''.flac", SearchOption.AllDirectories)
//
//test |> Array.iter (fun x -> printfn "%s" x)

//[<AutoOpen>]
//module MonadWriter = 
// 
//     type Writer<'a, 'L> = AWriter of 'a * List<'L>
// 
//     let bind = function 
//         | (v, itemLog) -> AWriter(v, [itemLog])
// 
//     let run = function
//         | AWriter(a, log) -> (a, log)
// 
//     let map fx = function
//         | AWriter(a, log) -> AWriter(fx a, log)
// 
//     let flatMap fx = function
//         | AWriter(a, log) -> 
//             let (v, new_log) = run (fx a)
//             AWriter(v, List.append log new_log)
//
//let sum x y = bind (x + y, "sum")
//let mul x y = bind (x * y, "mul")
//let mod1 x y = bind (x % y, "mod")
//let minus x y = bind (x - y, "minus")
//             
//let result = 
//         sum 5 5 
//         |> flatMap (mul 2) 
//         |> flatMap (mod1 25) 
//         |> flatMap (minus 10)
//
//let (v, log) = run result
//System.Console.WriteLine v
//for i in log do
//    System.Console.WriteLine i    