open Printf
open Scanf

let () =
  let n, q = scanf "%d %d " (fun x y -> (x, y)) in
  let st = Array.make (4*n) 0 in
  let add s t x =
    let rec iter k l r =
      if t <= l || r <= s then ()
      else if s <= l && r <= t then
        st.(k) <- st.(k) + x
      else
        let m = (l + r) / 2 in
        iter (2*k) l m;
        iter (2*k+1) m r in
    iter 1 0 n in
  let get i =
    let rec iter k l r s =
      if i = l && i+1 = r then s + st.(k)
      else
        let m = (l + r) / 2 in
        if i < m then iter (2*k) l m (s + st.(k))
        else iter (2*k+1) m r (s + st.(k))
    in
    iter 1 0 n 0 in
  let rec loop c =
    if c = 0 then ()
    else
      begin
        begin
          let com = scanf "%d " (fun a -> a) in
          if com = 0 then
            let s, t, x = scanf "%d %d %d " (fun a b c -> (a-1, b, c)) in
            add s t x
          else
            let i = scanf "%d " (fun a -> a-1) in
            get i |> printf "%d\n"
        end;
        loop (c-1)
      end
  in loop q
