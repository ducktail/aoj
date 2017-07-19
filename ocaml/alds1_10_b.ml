open Printf
open Scanf

let id x = x
             
let () =
  let n = scanf "%d\n" id in
  let dp = Array.make_matrix n n 0 in
  let mxa = Array.init n (fun _ -> scanf "%d %d\n" (fun x y -> (x, y))) in
  let rec iter i j =
    if i = 0 && j = n then dp.(0).(n-1)
    else if j = n then iter 0 (n-i+1)
    else
      let rec min_mc mc k =
        if k = j then mc
        else let tmc = dp.(i).(k) + dp.(k+1).(j) + fst mxa.(i) * snd mxa.(k) * snd mxa.(j) in
             min_mc (min mc tmc) (k+1)
      in
      begin
        dp.(i).(j) <- min_mc max_int i;
        iter (i+1) (j+1)
        end
  in iter 0 1 |> printf "%d\n"
