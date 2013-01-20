//pb3
(*The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ? *)
open System

let isPrime (x:bigint) = 
   seq {2I .. 1I .. x/2I} 
   |> Seq.map (fun i -> x % i = 0I)
   |> Seq.forall (fun p -> p = false)

let largestPrimeFactor (n:bigint) =
   seq {(bigint (Math.Sqrt (float n))) .. -(1I)..2I}
   |> Seq.tryFind (fun p -> n%p = 0I && isPrime p = true)   
   
let computeMaxPrimeFor n =
   let x = largestPrimeFactor n 
   match x with
    | None -> n
    | Some x -> x 
    
//and now we wait...
computeMaxPrimeFor 600851475143I |> printfn "Solution for problem 3 is %A"
