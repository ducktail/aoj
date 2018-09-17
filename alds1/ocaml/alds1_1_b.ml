open Printf
open Scanf

let rec gcd x y =
  if y = 0 then x else gcd y (x mod y)
                         
let () = scanf "%d %d\n" gcd |> printf "%d\n"
