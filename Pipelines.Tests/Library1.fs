namespace Pipeline.Tests
open Xunit
open Pipelines
open Pipelines.Pipeline
open Pipelines.Attempt

type Tests() = 
    [<Fact>]
    member this.TrivialTest () = 
        ()

    [<Fact>]
    member this.ZeroStepTest () = 
        let add1 x = x+1
        let d s =pipeline {
                    let! gg = s >?> add1 
                    return gg
                }
        let att=d 2
        Xunit.Assert.Equal(Success(3),att)
        ()

    [<Fact>]
    member this.MultiStepTest () = 
        let add1 x = x + 1
        let d (s:int) =pipeline { 
                    let! f = s >?> add1
                    let! g = f >?> add1 
                    let! h = g >?> add1 
                    let ddd = 1
                    return h
                }
        
        let r=d 3
        Xunit.Assert.Equal(Success(6),r)
        ()

    [<Fact>]
    member this.MultiStepFailTest () = 
        let mutable count = 0;
        let add1 x = 
                count<-count+1
                x + 1
        let ex = new System.Exception();
        let iThrow x = raise ex

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    let! g = f >?> iThrow 
                    let! h = g >?> add1 

                    return h
                }
        
        let r=d 3
        Xunit.Assert.Equal(Failure(ex),r)
        Xunit.Assert.Equal(1,count)
        ()


    [<Fact>]
    member this.OneStepTest () = 
        let add1 x = x + 1

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    return f
                }
        
        let r=d 3
        Xunit.Assert.Equal(Success(4),r)
        ()


    [<Fact>]
    member this.FailTest () = 
        let ex = new System.Exception();
        let add1 x = raise ex

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    return f
                }
        
        let r=d 3
        Xunit.Assert.Equal(Failure(ex),r)
        ()
    [<Fact>]
    member this.FailureStopsPipeTest () = 
        let ex = new System.Exception();
        let add1 x = raise ex
        let add2 x = x + 2

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    let! g = f >?> add2
                    return g
                }
        
        let r=d 3
        Xunit.Assert.Equal(Failure(ex),r)
        ()
    
