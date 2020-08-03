module SongsBackup.Api

open SongsBackup.IO
open SongsBackup.Types
open Common
open SongsBackup.DiskReader
open IO.Folder
open IO.Persistence
open SongsBackup.Types.Errors

let private fileGenerator (source) (dest) =
    result {
        let! path = Paths.createValidPath (source) |> Result.ofOption InvalidSource
        let files = searchAudio getFolderInfo path
        let! destPath = Paths.createValidPath (dest) |> Result.ofOption InvalidDestiny
        return saveAsJson files destPath
    }

let getSongInfoFromFile path =
    result {
        let! jsonPath = Paths.createValidJsonPath path |> Result.mapError validationToProcessError
        let! info = Reading.loadFromJson jsonPath |> Result.mapError fileToProcessError
        return info }

let generateAudioFileInfo source dest =
    fileGenerator source dest |> Result.mapError (fun e -> PathIsInvalid e |> fileToProcessError)

let searchFromJson (jsonPath: ValidJsonPath) (rootSearch: ValidPath) =
    result {
        let! info = Reading.loadFromJson jsonPath |> Result.mapError Errors.fileToProcessError
        return Search.getFiles info rootSearch }

module BackUp =
    type BackUpParams =
        { searchRoot: string
          jsonLocation: string
          destinationRoot: string }

    type BackupPaths =
        { searchRoot: ValidPath
          jsonLocation: ValidJsonPath
          destinationRoot: ValidPath }

    let validateParams (``params``: BackUpParams) =

        let create searchRoot jsonLocation destinationRoot: BackupPaths =
            { searchRoot = searchRoot
              jsonLocation = jsonLocation
              destinationRoot = destinationRoot }

        let searchRootOrError =
            ``params``.searchRoot
            |> Paths.createValidPath
            |> Result.ofOption (PathIsInvalid InvalidSource)
            |> Result.mapError Errors.fileToProcessError
            |> Validation.ofResult

        let jsonLocationOrError =
            ``params``.jsonLocation
            |> Paths.createValidJsonPath
            |> Result.mapError Errors.validationToProcessError
            |> Validation.ofResult

        let destinationRootOrError =
            ``params``.destinationRoot
            |> Paths.createValidPath
            |> Result.ofOption (PathIsInvalid InvalidSource)
            |> Result.mapError Errors.fileToProcessError
            |> Validation.ofResult

        (Validation.lift3 create) searchRootOrError jsonLocationOrError destinationRootOrError
   
    let backUp (paths: BackupPaths) =
        let mapper destination =
            function
            | SearchInfo a -> Search.copyFiles destination a
            | Empty -> AHybridResult([], [])

        result {

            let! baseP = searchFromJson paths.jsonLocation paths.searchRoot
            return! baseP
                    |> List.map (mapper paths.destinationRoot)
                    |> HybridResult.sequence
                    |> HybridResult.toResult
                    |> Result.map (fun x -> x.Length, List.sum x )
                    |> Result.mapError searchFileErrorsToProcessError
        }
        
     


    let generateBackup parameters =
        let flatten =
            function
            | Ok x ->
                match x with
                | Ok okR -> Ok okR
                | Error errR -> Error [ errR ]
            | Error e -> Error e

        parameters
        |> validateParams
        |> Validation.map backUp
        |> Validation.toResult
        |> flatten
