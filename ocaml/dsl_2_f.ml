open Printf
open Scanf

let () =
  let n, q = scanf "%d %d " (fun u v -> (u, v)) in
  let bs = 300 in
  let bn = (n + bs - 1) / bs in
  let iv = max_int in
  (* let iv = 2147483647 in *)
  let ar = Array.make (bs * bn) iv in
  let ubk = Array.make bn None in
  let mbk = Array.make bn iv in
  let wb i = match ubk.(i) with
      None -> ()
    | Some v -> let rec iter j =
                  if j = bs then ubk.(i) <- None
                  else begin
                      ar.(i*bs+j) <- v;
                      iter (j+1)
                    end in
                iter 0;
                mbk.(i) <- v
  in
  let fm i = match ubk.(i) with
      Some _ -> ()
    | None -> let rec iter j m =
                if j = bs then mbk.(i) <- m
                else iter (j+1) (min m ar.(i*bs+j)) in
              iter 0 iv
  in
  let update s t x =
    let rec iter i =
      if i >= t then ()
      else
        if i mod bs = 0 && i + bs <= t then
          begin
            ubk.(i / bs) <- Some x;
            mbk.(i / bs) <- x;
            iter (i + bs)
          end
        else
          begin
            wb (i / bs);
            ar.(i) <- x;
            iter (i + 1)
          end
    in
    let fma sb tb =
      let rec loop i =
        if i > tb then ()
        else begin
            fm i;
            loop (i+1)
          end in
      loop sb
    in
    iter s;
    fma (s / bs) ((t-1) / bs)
  in
  let find s t =
    let rec iter i m =
      if i >= t then m
      else
        if i mod bs = 0 && i + bs <= t then
          iter (i + bs) (min m mbk.(i / bs))
        else
          let u = match ubk.(i / bs) with
              None -> ar.(i)
            | Some v -> v in
          iter (i + 1) (min m u)
    in
    iter s iv
  in
  let rec loop c =
    if c = 0 then ()
    else
      begin
        (let com = scanf "%d " (fun u -> u) in
        if com = 0 then
          let s, t, x = scanf "%d %d %d " (fun u v w -> (u, v+1, w)) in
          update s t x
        else
          let s, t = scanf "%d %d " (fun u v -> (u, v+1)) in
          find s t |> printf "%d\n");
        loop (c - 1)
      end
  in
  loop q
