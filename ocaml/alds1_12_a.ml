open Printf
open Scanf

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

  let max_heapify pq i =
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
    iter i
  
  let pop pq =
    let ret = pq.hp.(1) in
    pq.hp.(1) <- pq.hp.(pq.cnt);
    pq.cnt <- pq.cnt - 1;
    max_heapify pq 1;
    ret

  let top pq = pq.hp.(1)
                    
  let is_empty pq = pq.cnt = 0
                               
end

let prim g n =
  let pq = Pqueue.create (n*n) (0, 0) (fun x y -> compare (fst y) (fst x)) in
  let vt = Array.make n true in
  let rec loop d =
    if Pqueue.is_empty pq then d
    else let w, u = Pqueue.pop pq in
         if vt.(u) then begin
             vt.(u) <- false;
             Array.iteri (fun v w' ->
                          if w' >= 0 then
                            Pqueue.push (w', v) pq
                         ) g.(u);
             loop (d+w)
           end
         else loop d
  in
  Pqueue.push (0,0) pq;
  loop 0

let id x = x
             
let () =
  let n = scanf "%d " id in
  let g = Array.init n (fun _ ->
                        Array.init n (fun _ ->
                                      let x = scanf " %d" id in x)
                       ) in
  printf "%d\n" (prim g n)
