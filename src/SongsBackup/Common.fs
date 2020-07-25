module SongsBackup.Common

type Validation<'Success, 'Failure> = Result<'Success, 'Failure list>
type HybridResult<'Processed, 'Error> = AHybridResult of List<'Processed> * List<'Error>

module HybridResult =
    let bind = function
        | (p, e) -> AHybridResult([p],[e])     
   
   
    let map f = function
       | AHybridResult(a,b) -> AHybridResult(f a, b)   
     
    let mapError f = function
       | AHybridResult(a,b) -> AHybridResult(a, f b)
    
    let doubleMap fO fE = function
       | AHybridResult(a,b) -> AHybridResult(fO a, fE b)
      
    let addErrors lE =  function
       | AHybridResult(a,b) -> AHybridResult(a, b @ lE)
       
    let combine hA hB =
        match hA, hB with
        | AHybridResult(a,b), AHybridResult(c,d) -> AHybridResult(a@c,b@d )
        
    let ofTuple (a,b) = AHybridResult(a,b)
    
    let private folder result acc =
            let (oks, errs) = acc
            match result with
            | Ok x -> x :: oks, errs
            | Error e -> oks, e :: errs
    
    let ofListResults  (results: Result<_, _> list) =
        let initialValue = [], []    
        List.foldBack folder results initialValue
        |> ofTuple
        
    let ofSeqResults  (results: Result<_, _> seq) =
        let initialValue = [], []    
        Seq.foldBack folder results initialValue
        |> ofTuple
    
    
    let unpack = function
         | AHybridResult(a,b) -> (a, b)
         
    let unpackErrors = function
        | AHybridResult(_, e) -> e
        
    let toResult = function
        | AHybridResult(a, b) -> if List.isEmpty b then
                                    Ok a
                                 else
                                    Error b
    
    let sequence aListHybridResult =       
        let initialValue = AHybridResult([],[])
        let folder (hybridResult: HybridResult<_,_>) (acc: HybridResult<_,_>) =
          combine hybridResult acc        
        let final = List.foldBack folder aListHybridResult initialValue
        final
        
[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Result.xxx` prefix to be used
module Result =
    let apply fR xR =
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1
        
    let sequence aListOfResults =
        let (<*>) = apply // monadic
        let (<!>) = Result.map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfResults initialValue    
   
    
      /// Apply a monadic function to an Result<x option>
    let bindOption f xR =
        match xR with
        | Some x -> f x |> Result.map Some
        | None -> Ok None

    /// Convert an Option into a Result. If none, use the passed-in errorValue
    let ofOption errorValue opt =
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    /// Convert a Result into an Option
    let toOption xR =
        match xR with
        | Ok v -> Some v
        | Error _ -> None

    /// Convert the Error case into an Option
    /// (useful with List.choose to find all errors in a list of Results)
    let toErrorOption =
        function
        | Ok _ -> None
        | Error err -> Some err
        
[<RequireQualifiedAccess>] // RequireQualifiedAccess forces the `Validation.xxx` prefix to be used
module Option =
  let apply fO fR =
        match fO, fR with
        | Some f, Some x -> Some (f x)
        | None , Some _ -> None
        | Some _, None -> None
        | None, None -> None
        
  let sequence aListOfOptions =
      let (<*>) = apply
      let (<!>) = Option.map
      let cons head tail = head :: tail
      let consR headR tailR = cons <!> headR <*> tailR
      let initialValue = Some [] // empty list inside Result
      List.foldBack consR aListOfOptions initialValue
      
  let flatten a =
       match a with
       | None          -> None
       | Some innerOpt -> innerOpt
      
[<AutoOpen>]
module OptionComputation =
    type OptionBuilder() =
        member __.Return(value) =           
            Some value

        member __.Bind(m, f) =           
            Option.bind f m

        member __.Zero() =           
            None

        member __.ReturnFrom(m: 'a option) =             
            m

    let maybe = OptionBuilder()

/// Functions for the `Validation` type (mostly applicative)
[<RequireQualifiedAccess>] // RequireQualifiedAccess forces the `Validation.xxx` prefix to be used
module Validation =

    let map f (x: Validation<_, _>): Validation<_, _> = Result.map f x

    let bind f (x: Validation<_, _>): Validation<_, _> = Result.bind f x

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV: Validation<_, _>) (xV: Validation<_, _>): Validation<_, _> =
        match fV, xV with
        | Ok f, Ok x -> Ok(f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error(errs1 @ errs2)

    //-----------------------------------
    // Lifting

    /// Lift a two parameter function to use Validation parameters
    let lift2 f x1 x2 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Validation parameters
    let lift3 f x1 x2 x3 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Validation parameters
    let lift4 f x1 x2 x3 x4 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Lift a five parameter function to use Validation parameters
    let lift5 f x1 x2 x3 x4 x5 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4 <*> x5


    // combine a list of Validation, applicatively
    let sequence (aListOfValidations: Validation<_, _> list) =
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfValidations initialValue

    //-----------------------------------
    // Converting between Validations and other types

    let ofResult xR: Validation<_, _> = xR |> Result.mapError List.singleton

    let toResult (xV: Validation<_, _>): Result<_, _> = xV

[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind(body(), (fun () -> this.While(guard, body)))

        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body())
            finally
                compensation()

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally
                (body',
                 (fun () ->
                     match disposable with
                     | null -> ()
                     | disp -> disp.Dispose()))

        member this.For(sequence: seq<_>, body) =
            this.Using
                (sequence.GetEnumerator(),
                 (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))

        member this.Combine(a, b) = this.Bind(a, (fun () -> b()))

    let result = ResultBuilder()
