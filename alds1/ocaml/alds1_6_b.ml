open Printf
open Scanf

let id x = x

let read_array n =
  Array.init n (fun i -> if i = n-1 then scanf "%d\n" id else scanf "%d " id)

let print_array a n bi =
  Array.iteri (fun i x -> if i = n-1 then begin
                              if i = bi then printf "[%d]\n" x
                              else printf "%d\n" x
                            end             
                          else begin
                              if i = bi then printf "[%d] " x
                              else printf "%d " x
                            end
              ) a

let partition a p r =
  let swap i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t in
  let x = a.(r) in
  let rec iter i j =
    if j = r then begin swap (i+1) r; i+1 end
    else if a.(j) <= x then
      let ii = i+1 in
      swap ii j;
      iter ii (j+1)
    else iter i (j+1) in
  iter (p-1) p
  
let () =
  let n = scanf "%d\n" id in
  let ar = read_array n in
  let i = partition ar 0 (n-1) in
  print_array ar n i
