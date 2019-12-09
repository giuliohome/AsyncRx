module Async = 

    let map f xAsync = async {
        // get the contents of xAsync 
        let! x = xAsync 
        // apply the function and lift the result
        return f x
        }

    let retn x = async {
        // lift x to an Async
        return x
        }

    let apply fAsync xAsync = async {
        // start the two asyncs in parallel
        let! fChild = Async.StartChild fAsync
        let! xChild = Async.StartChild xAsync

        // wait for the results
        let! f = fChild
        let! x = xChild 

        // apply the function to the results
        return f x 
        }

    let bind f xAsync = async {
        // get the contents of xAsync 
        let! x = xAsync 
        // apply the function but don't lift the result
        // as f will return an Async
        return! f x
        }

    let toOption computation = map Some computation
    let delay computation = async { 
        return! computation ()
        }
    let delayOption computation = 
        delay computation
        |> toOption 
    let toUpdate<'a,'b> (f: unit -> Async<'a>) (update: 'a -> 'b) 
        : unit -> Async<'b>  = 
        fun () -> 
            async {
                let! (res : 'a) = f ()
                return update res  
            }
    let toResult<'a,'b> (f: unit -> Async<'a>)
        : unit -> Async<Result<'a,'b>> =
        fun () ->
            async {
                let! (res : 'a) = f ()
                return Result.Ok res  
                }        

open System

[<EntryPoint>]
let main argv =
    let mutable test = DateTime.UtcNow
    let computation date = 
        async {
            return string date
        }
    let mappedDoc = 
        computation test
        |> Async.toOption 
    let boundDoc = 
        fun () -> computation test
        |> Async.delayOption 
    test <- test.AddDays(1.)
    let mapped = 
        mappedDoc 
        |> Async.RunSynchronously
        |> Option.toObj
    let bound =
        boundDoc
        |> Async.RunSynchronously
        |> Option.toObj

    sprintf "mapped = %s vs bound = %s" mapped bound
    |> System.Diagnostics.Debug.WriteLine
    0
