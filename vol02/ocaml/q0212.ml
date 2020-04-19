open Printf
open Scanf

let id x = x

let rec seq s t f =
  if s > t then []
  else
    let x = f s in
    x :: seq (s + 1) t f
             
module Pqueue = struct
  type 'a t = {mutable cnt : int; hp : 'a array; cmp : 'a -> 'a -> int}

  let create n v f = {cnt = 0; hp = Array.make n v; cmp = f}

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

let () = 
  let rec loop () = 
    let c, n, m, s, d = scanf "%d %d %d %d %d " @@ fun c n m s d -> (c, n, m, s, d) in
    if c = 0 && n = 0 && m = 0 && s == 0 && d = 0 then ()
    else
      begin
        let adj = Array.make (n + 1) [] in
        for i = 1 to m do
          let a, b, f = scanf "%d %d %d " @@ fun a b f -> (a, b, f) in
          adj.(a) <- (b, f) :: adj.(a);
          adj.(b) <- (a, f) :: adj.(b)
        done;
        let cost = Array.make_matrix (n + 1) (c + 1) 2000000 in
        let pq = Pqueue.create (n * n * c * 4) (0, 0, 0) (fun (_, _, x0) (_, _, x1) -> compare x1 x0) in
        Pqueue.push (s, c, 0) pq;
        let rec iter () =
          if Pqueue.is_empty pq then ()
          else begin
              let (i, j, k) = Pqueue.pop pq in
              if cost.(i).(j) > k then begin
                  cost.(i).(j) <- k;
                  List.iter (fun (e, f) -> Pqueue.push (e, j, k + f) pq;
                                           if j > 0 then Pqueue.push (e, (j-1), k + f / 2) pq else ()
                            ) adj.(i);
                end else ();
              iter ()
            end
        in iter ();
           printf "%d\n" @@ List.fold_left (fun x i -> min x cost.(d).(i)) 2000000 (seq 0 c id) ;
        loop ()
      end
  in loop ()
