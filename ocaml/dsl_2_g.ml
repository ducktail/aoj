open Printf
open Scanf

type ap = {a:int; p:int}

let () =
  let n, q = scanf "%d %d " (fun a b -> (a, b)) in
  let st = Array.make (4*n) {a=0; p=0} in
  let add s t x =
    let rec iter k l r =
      if r <= s || t <= l then ()
      else
        if s <= l && r <= t then
          st.(k) <- {st.(k) with a = st.(k).a + x}
        else
          begin
            let v = (min t r - max s l) * x in
            st.(k) <- {st.(k) with p = st.(k).p + v};
            let m = (l + r) / 2 in
            iter (2 * k) l m;
            iter (2 * k + 1) m r
          end
    in iter 1 0 n
  in
  let get_sum s t =
    let rec iter k l r =
      if r <= s || t <= l then 0
      else
        if s <= l && r <= t then
          (r - l) * st.(k).a + st.(k).p
        else
          let m = (l + r) / 2 in
          (min t r - max s l) * st.(k).a +
            iter (2 * k) l m + iter (2 * k + 1) m r
    in iter 1 0 n
  in
  let rec iter u =
    if u = 0 then ()
    else
      begin
        (let com = scanf "%d " (fun a -> a) in
         if com = 0 then
           let s, t, x = scanf "%d %d %d " (fun a b c -> (a-1, b, c)) in
           add s t x
         else
           let s, t = scanf "%d %d " (fun a b -> (a-1, b)) in
           get_sum s t |> printf "%d\n");
        iter (u-1)
      end
  in
  iter q
