namespace Pipelines

type StepResult<'TResult,'TError> =
        | Success of 'TResult
        | Failure of 'TError

type Attempt<'TResult> = (unit->StepResult<'TResult,System.Exception>)
    
module Attempt=
    let private TryCatch handler f x =
                    try
                        Success (f x)
                    with (ex)->
                        Failure (ex |> handler)

    let private SimpleTryCatch f x = TryCatch (fun x->x) f x

    let Attempt f x =
        fun () -> SimpleTryCatch f x

    let (>?>) x f =
        Attempt f x

module UnitOfWork=
    let MakeAtomic (attempt:Attempt<'t>) : Attempt<'t> =
        let transact () =
            use scope = new System.Transactions.TransactionScope()
            let res =attempt()
            match res with
                | Success(s)->
                    scope.Complete()
                | _->
                    ()
            res
        transact 


module PipelineBuilder=

    let private succeed x =(fun()->Success(x))
    let private fail ex = (fun()->Failure(ex))
    let private runAttempt (a:Attempt<'T>) = a()
    let private bind p rest = match runAttempt p with Failure ex -> fail ex | Success r -> (rest r)
    let private delay f = (fun () -> runAttempt(f()))
    let private combine p1 p2 = (fun () -> match p1() with Failure(e)-> p2() | res->res)

    type PipelineBuilder() = 
            member  __.Bind (p,rest) = bind p rest
            member  __.Delay (f) =delay f
            member  __.Return (x) =  succeed x
            member  __.ReturnFrom (x:Attempt<'TResult>) = x
            member  __.Combine (p1:Attempt<'TResult>,p2:Attempt<'TResult>) = combine p1 p2
            member  __.Zero () = fail (new System.Exception("No Work Specifiec"))

module Pipeline =
    open PipelineBuilder
    let pipeline = new PipelineBuilder()




