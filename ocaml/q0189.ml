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

module A = struct
  include Array

  let fold_lefti f iv ar = snd (fold_left (fun (i, r) v -> (i+1, f i r v)) (0, iv) ar)
end

let () =
  let solve n =
    let tbl = Wf.make 10 in
    let rec read x =
      if x = 0 then ()
      else
        let a, b, c = scanf "%d %d %d " (fun x y z -> (x, y, z)) in
        Wf.set tbl a b c;
        Wf.set tbl b a c;
        read (x - 1)
    in
    read n;
    Wf.calc tbl;
    A.fold_lefti (fun i (mi, mc) a ->
                  let sm = A.fold_left (fun ac c ->
                                        match c with
                                          Wf.Inf -> ac
                                        | _ -> Wf.add c ac
                                       ) (Wf.C 0) a in
                  match sm with
                    Wf.C 0 -> (mi, mc)
                  | _ when Wf.compare sm mc < 0 -> (i, sm)
                  | _ -> (mi, mc)
                 ) (-1, Wf.Inf) (snd tbl) |> (fun (x, y) -> printf "%d %d\n" x (Wf.cost y))
  in
  let rec loop () =
    let n = scanf "%d " (fun x -> x) in
    if n = 0 then ()
    else begin
        solve n;
        loop ()
      end
  in loop ()
