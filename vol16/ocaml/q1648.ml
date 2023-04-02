open Printf
open Scanf

let () =
  let fina a = Array.fold_left (fun c x -> if x = 0 then c + 1 else c) 0 a = 3 in
  let mina a = let (_, mi, mv) =
                 Array.fold_left (fun (i, mi, mv) x -> if x > 0 && x < mv then (i + 1, i, x) else (i + 1, mi, mv))
                   (0, 100, 200) a in (mi, mv) in
  let suba a i x =
    List.iter (fun j -> if i = j || a.(j) = 0 then () else a.(j) <- a.(j) - x) [0;1;2;3] in
  let rec solve a =
    if fina a then printf "%d\n" (Array.fold_left (+) 0 a)
    else
      begin
        (let (mi, mv) = mina a in suba a mi mv) ;
        solve a
      end in
  let rec loop () =
    let id x = x in
    let aa = Array.init 4 @@ fun _ -> scanf "%d " id in
    if Array.fold_left (+) 0 aa = 0 then ()
    else begin
        solve aa ;
        loop ()
      end
  in loop ()
