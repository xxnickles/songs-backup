// Learn more about F# at http://fsharp.org

open SongsBackup.UI.ConsoleUI

[<EntryPoint>]
let main _ =
    loop ShowMainMenu Initial
    0