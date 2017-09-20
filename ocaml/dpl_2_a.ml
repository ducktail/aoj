open Printf
open Scanf

module P = struct
  type path = Len of int | Inf | Unk

  let min l1 l2 =
    match l1, l2 with
      Len x, Len y when x < y -> Len x
    | Len x, Len y -> Len y
    | Len x, _ -> Len x
    | _, Len y -> Len y
    | _, _ -> Inf
                
  let add l1 l2 =
    match l1, l2 with
      Len x, Len y -> Len (x + y)
    | _, _ -> Inf
                
  let get = function
      Len x -> x
    | Inf -> (-1)
    | _ -> failwith "get"
end
             
let () =
  let nv, ne = scanf "%d %d " (fun x y -> (x, y)) in
  let g = Array.make_matrix nv nv P.Inf in
  let memo = Array.make_matrix (1 lsl nv) nv P.Unk in
  let set_g n =
    let rec loop x =
      if x = 0 then ()
      else let s, t, d = scanf "%d %d %d " (fun x y z -> (x, y, z)) in
           g.(s).(t) <- P.Len d;
           loop (x-1) in
    loop n in
  let rec search s v =
    match memo.(s).(v) with
      P.Unk -> 
      let rec loop u l =
        if u = nv then l
        else
          if s lsr u land 1 = 0 then loop (u+1) (P.min l (P.add g.(v).(u) (search (s lor (1 lsl u)) u)))
          else loop (u+1) l
      in
      let ml = loop 0 P.Inf in
      memo.(s).(v) <- ml; ml
    | x -> x in
  set_g ne;
  memo.((1 lsl nv) - 1 ).(0) <- P.Len 0;
  printf "%d\n" (P.get (search 0 0))
