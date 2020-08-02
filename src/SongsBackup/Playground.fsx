//open System.Diagnostics
//open System.Threading;
open System.IO
//let stopWatch =  Stopwatch()
//stopWatch.Start()
//Thread.Sleep(1023);
//stopWatch.Stop()
//let ts = stopWatch.Elapsed
//
//let formattedTime = sprintf "%02i:%02i:%02i.%03i" ts.Hours ts.Minutes ts.Seconds (ts.Milliseconds / 10)
//formattedTime


let f = FileInfo(@"D:\other");
let drive = Path.GetPathRoot(f.FullName)
let driveInfo = DriveInfo(drive)

printfn "%A" driveInfo.AvailableFreeSpace