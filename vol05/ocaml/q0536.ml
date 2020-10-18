open Printf
open Scanf

let id x = x

let rec take_cards n xs =
  let (a, b) = List.hd xs in
  let l = b - a + 1 in
  if n = l then ([(a, b)], List.tl xs)
  else if n < l then ([(a, a + n - 1)], (a + n, b)::List.tl xs)
  else let (cs, ds) = take_cards (n - l) (List.tl xs)
       in ((a, b)::cs, ds)

let shuffle cs (x, y) =
  let (is, js) = take_cards x cs in
  let (ks, ls) = take_cards (y - x) js in
  ls @ (ks @ is)

let count_cards r cs =
  let f ct (a, b) =
    if r < a then ct
    else if r <= b then ct + r - a + 1
    else ct + b - a + 1 in
  List.fold_left f 0 cs

let solve n p q r xys =
  let is = Array.fold_left shuffle [(1, n)] xys in
  let (_, js) = take_cards (p - 1) is in
  let (ks, _) = take_cards (q - p + 1) js in
  count_cards r ks
  
let () = 
  let rec loop () = 
    let n = scanf "%d " id in
    if n = 0 then ()
    else
      begin
        let m = scanf "%d " id in
        let p = scanf "%d " id in
        let q = scanf "%d " id in
        let r = scanf "%d " id in
        let xys = Array.init m @@ fun _ -> scanf "%d %d " @@ fun x y -> (x, y) in
        solve n p q r xys |> printf "%d\n";
        loop ()
      end
  in loop ()
