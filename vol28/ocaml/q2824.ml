open Printf
open Scanf

let solve t d l ax =
  let aw = Array.make (t + 1) 0 in
  let rec loopw i =
    if i > t then ()
    else
      begin
        if ax.(i - 1) >= l then
          aw.(i) <- d
        else
          aw.(i) <- max (aw.(i - 1) - 1) 0 ;
        loopw (i + 1)
      end in
  let rec loopt i wt st =
    if i > t then wt
    else if aw.(i - 1) = 0 && aw.(i) > 0 then
      loopt (i + 1) wt i
    else if aw.(i - 1) > 0 && aw.(i) = 0 || i = t && aw.(i) > 0 then
      loopt (i + 1) (wt + i - st) st
    else
      loopt (i + 1) wt st in
  loopw 1 ;
  loopt 1 0 0

let () = 
  let rec loop () = 
    let t, d, l = scanf "%d %d %d " @@ fun t d l -> (t, d, l) in
    if t = 0 && d = 0 && l = 0 then ()
    else
      begin
        Array.init t (fun _ -> scanf "%d " @@ fun x -> x) |> solve t d l |> printf "%d\n" ;
        loop ()
      end
  in loop ()
