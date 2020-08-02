namespace SongsBackup.UI

open System.Diagnostics
open SongsBackup.Api
open SongsBackup.Types

module ConsoleUI =
    open System

    type State =
        | Initial
        | CreatedJson of string
        | GeneratedFromJson of string
        | Done

    type Msg =
        | ShowMainMenu
        | ShowGenerateFile
        | ShowBackUpFromPreviouslyCreated
        | ShowBackup
        | GenerateFile of string * string
        | CopyFromFile of string * string * string
        | ShowError of string
        | ShowMessage of string
        | Quit

    type UserInterface =
        { Show: string -> unit
          GetInputString: unit -> string }

    type Action =
        { key: string
          message: Msg }

    let basicMenuActions =
        [ { key = "q"
            message = Quit }
          { key = "1"
            message = ShowGenerateFile }
          { key = "2"
            message = ShowBackup } ]

    let extendedMenuAction =
        basicMenuActions @ [ { key = "3"
                               message = ShowBackUpFromPreviouslyCreated } ]

    let rec processAction actions input =
        match actions |> List.tryFind (fun action -> action.key = input) with
        | Some action -> action.message
        | None ->
            let validKeys =
                actions
                |> List.map (fun action -> action.key)
                |> String.concat ", "
            printfn "Invalid input. Please use one of the following options [%s]" validKeys
            let newInput = Console.ReadLine()
            processAction actions newInput

    let getGenerateFileParams () =
        printfn "Where to scan?"
        let org = Console.ReadLine()
        printfn "Where to create file?"
        let dest = Console.ReadLine()
        GenerateFile(org, dest)

    let private getBackUpParamsBase () =
        printfn "What is the search root?"
        let root = Console.ReadLine()
        printfn "Where to copy files?"
        let dest = Console.ReadLine()
        root, dest

    let getBackUpParamsFromCreatedJson jsonPath =
        let (root, dest) = getBackUpParamsBase ()
        CopyFromFile(root, jsonPath, dest)


    let getBackupParams () =
        printfn "Where is the json file located?"
        let jsonPath = Console.ReadLine()
        let (root, dest) = getBackUpParamsBase ()
        CopyFromFile(root, jsonPath, dest)


    let basicMainMenu () =
        printfn "Select an option (use q to exit)"
        printfn "(1) Generate JSON"
        printfn "(2) Generate Backup"

    type ExtendedType =
        | FromCreated
        | FromLastGenerated

    let extendedMenu menuType =
        basicMainMenu ()
        let baseMessage = sprintf "(3) Generate Backup from %s"

        let message =
            match menuType with
            | FromCreated -> baseMessage "last created JSON"
            | FromLastGenerated -> baseMessage "last generated"
        printfn "%s" message



    type MenuType =
        | Basic
        | Extended of ExtendedType


    let private getMenuOption menuType =
        let displayMenu =
            function
            | Basic -> basicMainMenu ()
            | Extended t -> extendedMenu t
        displayMenu menuType
        Console.ReadLine()


    let private invalidPathErrorToString =
        function
        | InvalidSource -> "Source path is invalid"
        | InvalidDestiny -> "Destiny path is invalid"

    let private searchFileErrorToString =
        function
        | CopyFileError e -> sprintf "File %s couldn't be copied" e.Value
        | FileNotFoundError f -> sprintf "%s was not found" f.Value


    let processErrorToString =
        function
        | Validation v ->
            match v with
            | StringIsEmpty -> "Provided string is empty"
            | InvalidJsonFile -> "Provided Json File is incorrect"
        | File e ->
            match e with
            | PathIsInvalid p -> invalidPathErrorToString p
            | SearchErrors errLst ->
                errLst
                |> List.map searchFileErrorToString
                |> String.concat "\n"
            | FileCouldNotBeLoaded -> "File couldn't be loaded"

    let private displayMsg msg color =
        Console.ForegroundColor <- color
        printfn "%s" msg
        Console.ResetColor()
        
    let private withTime f =
        let stopWatch =  Stopwatch()
        stopWatch.Start()
        let result = f()
        stopWatch.Stop()
        let ts = stopWatch.Elapsed
        let formattedTime = sprintf "%02i:%02i:%02i.%03i" ts.Hours ts.Minutes ts.Seconds (ts.Milliseconds / 10)
        displayMsg (sprintf "Elapsed %s" formattedTime) ConsoleColor.Cyan
        result
        

    let ``process`` msg state =
        match msg, state with
        | ShowMainMenu, Initial ->
            let newMessage = getMenuOption Basic |> processAction basicMenuActions
            newMessage |> Some, state
        | ShowMainMenu, CreatedJson _ ->
            let newMessage = getMenuOption (Extended FromCreated) |> processAction (extendedMenuAction)
            newMessage |> Some, state
        | ShowMainMenu, GeneratedFromJson _ ->
            let newMessage = getMenuOption (Extended FromLastGenerated) |> processAction (extendedMenuAction)
            newMessage |> Some, state
        | ShowGenerateFile, _ ->
            let newMessage = getGenerateFileParams ()
            newMessage |> Some, state
        | ShowBackUpFromPreviouslyCreated, CreatedJson json ->
            let newMessage = getBackUpParamsFromCreatedJson json
            newMessage |> Some, state
        | ShowBackUpFromPreviouslyCreated, GeneratedFromJson json ->
            let newMessage = getBackUpParamsFromCreatedJson json
            newMessage |> Some, state
        | ShowBackup, _ ->
            let newMessage = getBackupParams ()
            newMessage |> Some, state
        | GenerateFile (org, dest), _ ->
            let f () =
                let result = generateAudioFileInfo org dest
                match result with
                | Ok _ -> ShowMessage "Json backup file completed" |> Some, CreatedJson(sprintf "%s/data.json" dest)
                | Error e -> ShowError(processErrorToString e) |> Some, state
            withTime f          
        | CopyFromFile (root, jsonPath, dest), _ ->
            let f () =
                let result =
                    BackUp.generateBackup
                        { searchRoot = root
                          jsonLocation = jsonPath
                          destinationRoot = dest }
                match result with
                | Ok _ ->
                    sprintf "Files has been copied to %s" dest
                    |> ShowMessage
                    |> Some, GeneratedFromJson jsonPath
                | Error err ->
                    err
                    |> List.map processErrorToString
                    |> String.concat "\n"
                    |> ShowError
                    |> Some, state
            withTime f
        | ShowMessage msg, _ ->
            displayMsg msg ConsoleColor.Green
            ShowMainMenu |> Some, state
        | ShowError msg, _ ->
            displayMsg msg ConsoleColor.Red
            ShowMainMenu |> Some, state
        | Quit, _ -> None, Done
        | _ , Done -> failwith "Called with incorrect message"
        | _ , Initial -> failwith "Trying to initialize with incorrect message"

    let rec loop msg state =
        let (msg, state) = ``process`` msg state
        match msg with
        | Some m -> loop m state
        | None ->
            match state with
            | Done -> ()
            | _ -> failwith "Incorrect state"
