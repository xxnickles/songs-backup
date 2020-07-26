open System.Diagnostics
open System.Threading;

let stopWatch =  Stopwatch()
stopWatch.Start()
Thread.Sleep(1023);
stopWatch.Stop()
let ts = stopWatch.Elapsed

let formattedTime = sprintf "%02i:%02i:%02i.%03i" ts.Hours ts.Minutes ts.Seconds (ts.Milliseconds / 10)
formattedTime
