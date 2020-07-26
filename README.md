# Songs Backup
Personal toy project I used as initial PoC for learning f# and functional programing basics.

## Why?

I often use MicroSD card to store my music for mobile devices (music streaming and I are not good friends). Time to time I either lost the card (sometimes the whole device) or the card get damage and needs to be replaced. These kind of situations brings me a cumbersome problem: I have to manually recopy and reorganize my music in the new card. i really don't use any usual organization means, like playlist, because I rather prefer enjoy the random play in my devices.

## How it works?

I just wanted to have a basic reference of what I have in my music, I really don't want to have a duplicate (full backup) of the music I store in my mobile devices (which is a subset of my music library). For that reason, I came up with the following process:

1. Generate a JSON file with the folder / file structure of my music (audio files) in the SD Card. In that way, I will know which files I will need to transfer are where they were located. This "backup" is very cheap in terms of space and I will not require to have a full copy of the files that I already have in my local storage
2. Use this JSON file to look the music files in my local library, and then copy them to a new destination.

Pretty straightforward. 

## Requirements

- [Dotnet Core 3](https://dotnet.microsoft.com/download/dotnet-core)
- [Ionide (VS Code)](http://ionide.io/) or [Rider](https://www.jetbrains.com/rider/)


## Dependencies

- [Thoth.Json.Net](https://thoth-org.github.io/Thoth.Json/)
- [Paket](https://fsprojects.github.io/Paket/get-started.html)


## ToDo

 - Verify if the target location has enough free space to copy the files
 - Return additional information about the process (like number of copied files, time, etc)

    