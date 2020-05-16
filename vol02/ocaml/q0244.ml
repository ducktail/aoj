open Printf
open Scanf

module Pqueue = struct

  type 'a t = {mutable cnt : int; hp : 'a array; cmp : 'a -> 'a -> int}

  let create n v f =
    let rec g x = if n <= x then x + 1 else g (2 * x + 1) in
    {cnt = 0; hp = Array.make (g 3) v; cmp = f}

  let push k pq =
    let swap i j =
      let t = pq.hp.(i) in
      pq.hp.(i) <- pq.hp.(j);
      pq.hp.(j) <- t in
    let rec iter i =
      let p = i / 2 in
      if p = 0 || pq.cmp pq.hp.(p) pq.hp.(i) >= 0 then ()
      else (swap p i; iter p)
    in
    pq.cnt <- pq.cnt + 1;
    pq.hp.(pq.cnt) <- k;
    iter (pq.cnt)

  let max_heapify pq n =
    let swap i j =
      let t = pq.hp.(i) in
      pq.hp.(i) <- pq.hp.(j);
      pq.hp.(j) <- t in
    let rec iter i =
      let l = 2 * i in
      let r = 2 * i + 1 in
      let mi =
        let ti = if l <= pq.cnt && pq.cmp pq.hp.(l) pq.hp.(i) > 0 then l else i in
        if r <= pq.cnt && pq.cmp pq.hp.(r) pq.hp.(ti) > 0 then r else ti in
      if mi <> i then
        begin
          swap mi i;
          iter mi
        end
    in
    iter n

  let pop pq =
    let ret = pq.hp.(1) in
    pq.hp.(1) <- pq.hp.(pq.cnt);
    pq.cnt <- pq.cnt - 1;
    max_heapify pq 1;
    ret

  let top pq = pq.hp.(1)

  let is_empty pq = pq.cnt = 0

  let length pq = pq.cnt

end

let solve n m =
  let adj = Array.init n @@ fun _ -> [] in
  let pq = Pqueue.create (n * n * 3) (0, 0, 0) (fun (_, _, x) (_, _, y) -> compare y x) in
  let cost = Array.make_matrix n 3 1000000 in
  let rec loop () =
    if Pqueue.is_empty pq then ()
    else begin
        let (i, t, c) = Pqueue.pop pq in
        if c > cost.(i).(t) then loop ()
        else begin
            if t = 2 || t = 0 then
              begin
                List.iter (fun (j, w) ->
                           if cost.(j).(t) > cost.(i).(t) + w then
                             begin
                               cost.(j).(t) <- cost.(i).(t) + w;
                               Pqueue.push (j, t, cost.(j).(t)) pq
                             end else ()
                          ) adj.(i)
              end;
            if t = 2 || t = 1 then
              begin
                List.iter (fun (j, w) ->
                           if cost.(j).(t-1) > cost.(i).(t) then
                             begin
                               cost.(j).(t-1) <- cost.(i).(t);
                               Pqueue.push (j, t-1, cost.(j).(t-1)) pq
                             end else ()
                          ) adj.(i)
              end;
            loop ()
          end
      end in
  for i = 1 to m do
    let a, b, c = scanf "%d %d %d " @@ fun x y z -> (x-1, y-1, z) in
    adj.(a) <- (b, c) :: adj.(a);
    adj.(b) <- (a, c) :: adj.(b)
  done;
  cost.(0).(2) <- 0;
  Pqueue.push (0, 2, 0) pq;
  loop ();
  cost.(n-1).(0)

let () = 
  let rec loop () = 
    let n, m = scanf "%d %d " @@ fun n m -> (n, m) in
    if n = 0 && m = 0 then ()
    else
      begin
        solve n m |> printf "%d\n";
        loop ()
      end
  in loop ()
