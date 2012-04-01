//pb3
(*The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ? *)
open System
open System.Diagnostics
open System.Numerics
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Collections.Generic

/// Check if a number is prime
let isPrime (x:BigInteger) =
    let rec primeCheck count =
        // If the counter has reached x, then we know x is prime
        if count = x       then true
        // If x is divisible by the counter, we know x isn't prime
        elif x % count = BigInteger.Zero then false
        else primeCheck (count + BigInteger.One)
    // Special case 1 as prime
    if x = BigInteger.One 
    then true
    else primeCheck 2I
    
let generateDescendingBigIntSequence (maxValue:BigInteger) (minValue:BigInteger) = Seq.unfold (fun n -> if n < minValue then None else Some(n, n - 1I)) maxValue

let computeMaxPrimeFor targetNumber =    
    let tasksToSpawn = 10I
    let range = targetNumber/tasksToSpawn
    let primes = new ConcurrentBag<BigInteger>()

    let tasks =
        [|  
        for i in 0I .. tasksToSpawn - 1I do
            yield Task.Factory.StartNew(
                Action(fun () ->
                    generateDescendingBigIntSequence (range* (i+1I)) (range*i)
                    |> Seq.filter (fun x -> targetNumber%x = 0I)
                    |> Seq.filter isPrime
                    |> Seq.iter primes.Add
                )
            )
        |]
    Task.WaitAll(tasks)
    let primeSeq = primes :> seq<BigInteger>
    Seq.max primeSeq

let watch = new Stopwatch()
watch.Start()
//and now we wait...
computeMaxPrimeFor 600851475143I |> printfn "Solution for problem 3 is %A"
watch.Stop()
printfn "Max Factor found in %A seconds" (watch.ElapsedMilliseconds/1000L)