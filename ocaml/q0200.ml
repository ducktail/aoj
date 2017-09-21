open Printf
open Scanf

module Wf = struct
  type cost = C of int | Inf

  type t = int * cost array array

  let make n : t = let ar = Array.make_matrix n n Inf in
                   let rec loop i =
                     if i = n then ()
                     else begin
                         ar.(i).(i) <- C 0;
                         loop (i + 1)
                       end in
                   loop 0;
                   (n, ar)

  let set ((n, ar) : t) u v c = ar.(u).(v) <- C c

  let add x y =
    match x, y with
      C x, C y -> C (x + y)
    | _, _ -> Inf

  let compare x y =
    match x, y with
      C x, C y -> x - y
    | Inf, Inf -> 0
    | Inf, _ -> 1
    | _, Inf -> (-1)
                  
  let min x y =
    match compare x y with
      0 -> x
    | r when r > 0 -> y
    | _ -> x

  let max x y =
    match compare x y with
      0 -> x
    | r when r > 0 -> x
    | _ -> y
                
  let calc ((n, ar) : t) =
    let rec loop k i j =
      if k = n then ()
      else if i = n then loop (k + 1) 0 0
      else if j = n then loop k (i + 1) 0
      else let nc = add ar.(i).(k) ar.(k).(j) in
           ar.(i).(j) <- min nc ar.(i).(j);
           loop k i (j + 1) in
    loop 0 0 0

  let cost = function
      C x -> x
    | _ -> (-1)
end

let () =
  let solve n m =
    let ct = Wf.make m in
    let tt = Wf.make m in
    let rec read x =
      if x = 0 then ()
      else
        let a, b, c, t = scanf "%d %d %d %d " (fun w x y z -> (w-1, x-1, y, z)) in
        Wf.set ct a b c;
        Wf.set ct b a c;
        Wf.set tt a b t;
        Wf.set tt b a t;
        read (x-1)
    in
    read n;
    Wf.calc tt;
    Wf.calc ct;
    let k = scanf "%d " (fun x -> x) in
    let rec query x =
      if x = 0 then ()
      else
        let p, q, r = scanf "%d %d %d " (fun x y z -> (x-1, y-1, z)) in
        Wf.cost (if r = 0 then (snd ct).(p).(q) else (snd tt).(p).(q)) |> printf "%d\n";
        query (x - 1)
    in
    query k
  in
  let rec loop () =
    let n, m = scanf "%d %d " (fun x y -> (x, y)) in
    if n = 0 && m = 0 then ()
    else begin
        solve n m;
        loop ()
      end
  in loop ()
