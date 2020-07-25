namespace SongsBackup.Types

open System
open System.IO
open System.Text.RegularExpressions
open SongsBackup.Common

type CopyError =
    | CopyError of string
    member s.Value =
        match s with
        | CopyError v -> v

type FileNotFound =
    | FileNotFound of string
    member s.Value =
        match s with
        | FileNotFound v -> v

type InvalidPathError =
    | InvalidSource
    | InvalidDestiny

type ValidationError =
    | StringIsEmpty
    | InvalidJsonFile


type SearchFileError =
    | CopyFileError of CopyError
    | FileNotFoundError of FileNotFound

type FileError =
    | PathIsInvalid of InvalidPathError
    | SearchErrors of SearchFileError list
    | FileCouldNotBeLoaded

type ProcessError =
    | Validation of ValidationError
    | File of FileError


module Errors =
    let validationToProcessError (e: ValidationError) = Validation e

    let fileToProcessError (e: FileError) = File e

    let searchFileErrorsToProcessError (e: SearchFileError list) = SearchErrors e |> fileToProcessError


type NonEmptyString =
    private
    | NonEmptyString of string
    member s.Value =
        match s with
        | NonEmptyString v -> v

module NonEmptyString =
    let create s =
        if String.IsNullOrEmpty s then Error StringIsEmpty else Ok <| NonEmptyString s

type Filename = NonEmptyString

type ValidPath =
    private
    | ValidPath of string
    member s.Value =
        match s with
        | ValidPath v -> v

type ValidJsonPath =
    private
    | ValidJsonPath of string
    member s.Value =
        match s with
        | ValidJsonPath v -> v

type SimplePath =
    | SimplePath of string
    member s.Value =
        match s with
        | SimplePath v -> v
//                    member s.Value(SimplePath value) = value

module Paths =
    let private isValid (s) =
        try
            Directory.Exists s
        with ex -> false

    let createValidPath s =
        if isValid s then Some <| ValidPath s else None

    let createValidJsonPath s =
        if File.Exists s then
            let info = FileInfo s
            if info.Extension = ".json" then ValidJsonPath s |> Ok else Error InvalidJsonFile
        else
            Error InvalidJsonFile

    let combine (p1: ValidPath) (p2: SimplePath) = p1.Value + p2.Value |> SimplePath


type Extension =
    private
    | Extension of string
    member s.Value(Extension value) = value

module Extension =
    let create s =
        if Regex.IsMatch(s, """\.{1}(\w|\d){2,}""") then Some s else None


type FolderInfo =
    { Path: string
      Songs: string seq
      Subfolders: FolderInfo [] }
    member f.HasSongs = Seq.isEmpty f.Songs

[<CLIMutable>]
type FolderInfoDto =
    { Path: string
      Songs: string List
      Subfolders: FolderInfoDto List }


module FolderInfo =
    let rec toDto (folderInfo: FolderInfo): FolderInfoDto =

        { Path = folderInfo.Path
          Songs = List.ofSeq folderInfo.Songs
          Subfolders =
              folderInfo.Subfolders
              |> Array.map toDto
              |> List.ofArray }


    let rec fromDto (folderInfoDto: FolderInfoDto): FolderInfo =

        { Path = folderInfoDto.Path
          Songs = folderInfoDto.Songs |> Seq.ofList
          Subfolders =
              folderInfoDto.Subfolders
              |> List.map fromDto
              |> Array.ofList }



type SearchInfo =
    { destination: SimplePath
      results: HybridResult<SimplePath, SearchFileError> }

type SearchInfoResults =
    | SearchInfo of SearchInfo
    | Empty
