namespace Pipeline.Tests
open Xunit
open Pipelines
open Pipelines.Pipeline
open Pipelines.Attempt

type Class1() = 
    [<Fact>]
    member this.TrivialTest () = 
        ()

    [<Fact>]
    member this.ZeroStepTest () = 
        let pipeline = new PipelineBuilder()

        let d s =pipeline {   
                    return s
                }
        
        let att=d 3
        let r = att()
        Xunit.Assert.Equal(Success(3),r)
        ()

    [<Fact>]
    member this.MultiStepTest () = 
        let pipeline = new PipelineBuilder()
        let add1 x = x + 1

        let d (s:int) =pipeline { 
                    let! f = s >?> add1
                    let! g = f >?> add1 
                    let! h = g >?> add1 
                    let ddd = 1
                    return h
                }
        
        let att=d 3
        let r = att()
        Xunit.Assert.Equal(Success(6),r)
        ()

    [<Fact>]
    member this.MultiStepFailTest () = 
        let pipeline = new Pipeline.PipelineBuilder()
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
        
        let att=d 3
        let r = att ()
        Xunit.Assert.Equal(Failure(ex),r)
        Xunit.Assert.Equal(1,count)
        ()

    [<Fact>]
    member this.MultiStepDelayTest () = 
        let pipeline = new Pipeline.PipelineBuilder()
        let mutable count = 0;
        let add1 x = 
                count<-count+1
                x + 1

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    let! g = f >?> add1 
                    let! h = g >?> add1 

                    return h
                }

        Xunit.Assert.Equal(0,count)        
        let att=d 3
        Xunit.Assert.Equal(0,count)        
        let r = att()
        Xunit.Assert.Equal(3,count)
        ()


    [<Fact>]
    member this.OneStepTest () = 
        let pipeline = new Pipeline.PipelineBuilder()
        let add1 x = x + 1

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    return f
                }
        
        let att=d 3
        let r =  att()
        Xunit.Assert.Equal(Success(4),r)
        ()


    [<Fact>]
    member this.FailTest () = 
        let pipeline = new Pipeline.PipelineBuilder()
        let ex = new System.Exception();
        let add1 x = raise ex

        let d (s:int) =pipeline { 
                    let! f = s >?> add1 
                    return f
                }
        
        let att=d 3
        let r = att()
        Xunit.Assert.Equal(Failure(ex),r)
        ()
    
    [<Fact>]
    member this.ReturnDirect () = 
        let pipeline = new Pipeline.PipelineBuilder()

        let d (s:int) =pipeline { 
                    return! fun ()->Success(s)
                }
        
        let att=d 3
        let r = att()
        Xunit.Assert.Equal(Success(3),r)
        ()

