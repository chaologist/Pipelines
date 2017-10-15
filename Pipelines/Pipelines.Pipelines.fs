namespace Pipelines

type StepResult<'TResult,'TError> =
        | Success of 'TResult
        | Failure of 'TError

type Attempt<'TResult> = (unit->StepResult<'TResult,System.Exception>)
    
module Attempt=
    let TryCatch handler f x =
        try
            Success (f x)
        with (ex)->
            Failure (ex |> handler)

    let SimpleTryCatch f x = TryCatch (fun x->x) f x

    let Attempt f x =
        fun () -> SimpleTryCatch f x

    let (>?>) x f =
        Attempt f x
    

module Pipeline=

    let succeed x =(fun()->Success(x))
    let fail ex = (fun()->Failure(ex))
    let runAttempt (a:Attempt<'T>) = a()
    let bind p rest = match runAttempt p with Failure ex -> fail ex | Success r -> (rest r)
    let delay f = (fun () -> runAttempt(f()))
    let combine p1 p2 = (fun () -> match p1() with Failure(e)-> p2() | res->res)

    type PipelineBuilder() = 
            member __.Bind (p,rest) = bind p rest
            member __.Delay (f) =delay f
            member __.Return (x) =  succeed x
            member __.ReturnFrom (x:Attempt<'TResult>) = x
            member __.Combine (p1:Attempt<'TResult>,p2:Attempt<'TResult>) = combine p1 p2
            member __.Zero () = fail (new System.Exception("No Work Specifiec"))



