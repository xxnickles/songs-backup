module SongsBackup.DiskReader

open SongsBackup.Types
open SongsBackup.IO

type ReadErrors = SearchFailed of NonEmptyString


let audioExtensions =
    seq {
        yield ".ogg"
        yield ".mp3"
        yield ".flac"
        yield ".m4a"
        yield ".aac"
        yield ".mka"
    }


let searchAudio (f: FilesGetter) (path: ValidPath) = f audioExtensions path.Value
