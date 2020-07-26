module SongsBackup.IO

open System.IO
open System.Text
open SongsBackup.Common
open SongsBackup.Types

type FilesGetter = string seq -> string -> FolderInfo

type SaveFile = FolderInfo -> Async<unit>

module Folder =
    let private getFileName filePath =
        let fileInfo = FileInfo filePath
        fileInfo.Name

    let private getDirName dirPath =
        let dirInfo = DirectoryInfo dirPath
        dirInfo.Name

    let private getFolders path = Directory.GetDirectories(path, "*", SearchOption.TopDirectoryOnly)

    let private removeRoot (path: string) =
        let root = Path.GetPathRoot(path)
        path.[root.Length..path.Length - 1]

    let toOption seq =
        if Seq.isEmpty seq then None else Some seq

    let rec getFolderInfo extensions path: FolderInfo =
        let byExt (x: string) = extensions |> Seq.exists (fun s -> s = Path.GetExtension(x).ToLowerInvariant())

        let soundFiles =
            Directory.EnumerateFiles(path, "*.*", SearchOption.TopDirectoryOnly)
            |> Seq.cast<string>
            |> Seq.filter byExt
            |> Seq.map getFileName

        let subFolders = getFolders path

        let subInfo = subFolders |> Array.map (getFolderInfo extensions)

        { Path = path |> removeRoot
          Songs = soundFiles
          Subfolders = subInfo }

module Json =
    open Thoth.Json.Net

    let serializeJson a = Encode.Auto.toString (4, a)
    let deserializeJson<'a> (str: string) = Decode.Auto.fromString<'a> (str)


module Reading =
    open Json
    open SongsBackup.Types.FolderInfo

    let loadFromJson (dir: ValidJsonPath) =
        let dto = deserializeJson<FolderInfoDto> (File.ReadAllText(dir.Value))

        dto
        |> Result.map fromDto
        |> Result.mapError (fun _ -> FileCouldNotBeLoaded)


module Persistence =
    open Json
    open SongsBackup.Types.FolderInfo

    let private persist dir (content: string) =
        use writer = new System.IO.StreamWriter(sprintf "%s/data.json" dir)
        writer.Write content

    let saveAsJson (songs: FolderInfo) (dir: ValidPath) =
        songs
        |> toDto
        |> serializeJson
        |> persist dir.Value


module Search =

    type Finder = string -> Result<SimplePath, SearchFileError>

    let findSong (baseDir: ValidPath) song =
        printf "looking for %s in %s \n" song baseDir.Value
        let results = Directory.GetFiles(baseDir.Value, song, SearchOption.AllDirectories)
        if Array.isEmpty results
        then FileNotFoundError(FileNotFound song) |> Error
        else SimplePath results.[0] |> Ok

    let copy (source: SimplePath) (dest: SimplePath) =
        printf "trying to copy %s to %s \n" source.Value dest.Value
        try
            let fileName = Path.GetFileName(source.Value)
            let finalDest = Path.Combine(dest.Value, fileName)
            File.Copy(source.Value, finalDest, true) |> Ok
        with x -> CopyFileError(CopyError source.Value) |> Error

    let createDirectoryIfNotExist (dest: SimplePath) =
        let dir = System.IO.DirectoryInfo(dest.Value)
        dir.Create()


    let private getFilesWithFinder (finder: Finder) (songs: string seq) =
        songs
        |> Array.ofSeq
        |> Array.Parallel.map finder
        |> HybridResult.ofSeqResults

    let private mapOptionalHybrid e =
        match e with
        | Some a -> HybridResult.sequence a |> Some
        | None -> None

    let rec getFiles (f: FolderInfo) (baseDir: ValidPath) =
        let finder = findSong baseDir
        let songsGetter = getFilesWithFinder finder

        let folderMapper a =
            a
            |> Array.Parallel.map (fun f -> (getFiles f baseDir))
            |> List.ofArray
            |> List.concat

        let firstLevel =
            match Seq.isEmpty f.Songs with
            | false ->
                SearchInfo
                    { destination = SimplePath f.Path
                      results = songsGetter f.Songs }
            | true -> Empty

        let folders = folderMapper f.Subfolders

        [ firstLevel ] @ folders

    let copyFiles (destinationRoot: ValidPath) (searchResults: SearchInfo) =
        let finalPath = Paths.combine destinationRoot searchResults.destination
        createDirectoryIfNotExist finalPath
        let map (paths: SimplePath list) =
            paths
            |> Array.ofList
            |> Array.Parallel.map (fun x -> copy x finalPath)
            |> HybridResult.ofSeqResults

        let (paths, errors) = HybridResult.unpack searchResults.results

        paths
        |> map
        |> HybridResult.addErrors errors
