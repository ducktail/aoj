open Printf
open Scanf

let () =
  let n, q = scanf "%d %d " (fun x y -> (x, y)) in
  let st = Array.make 300000 0 in
  let update x y =
    let rec iter k l r =
      if l = x && r = (x+1) then begin
          st.(k) <- st.(k) + y;
        end
      else
        let m = (l + r) / 2 in
        if x < m then
          iter (2*k) l m
        else
          iter (2*k+1) m r;
        st.(k) <- st.(2*k) + st.(2*k+1);
    in iter 1 0 n
  in
  let query x y =
    let rec iter k l r =
      if y <= l || r <= x then 0
      else
        if x <= l && r <= y then st.(k)
        else
          let m = (l + r) / 2 in
          (iter (2*k) l m) + (iter (2*k+1) m r)
    in iter 1 0 n
  in
  let rec loop i =
    if i = 0 then ()
    else
      begin
        let com, x, y = scanf "%d %d %d " (fun a b c -> (a, b, c)) in
        if com = 0 then
          update (x-1) y
        else
          printf "%d\n" (query (x-1) y);
        loop (i-1)
      end
  in
  loop q
