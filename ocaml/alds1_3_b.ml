open Printf
open Scanf

let () =
  let n, q = scanf "%d %d\n" (fun x y -> (x, y)) in
  let queue = Queue.create () in
  let rec init_q x =
    if x = 0 then () else
      let d = scanf "%s %d\n" (fun a b -> (a, b)) in
      Queue.push d queue; init_q (x-1) in
  let rec iter ct =
    if Queue.is_empty queue then ()
    else begin
        let (n, t) = Queue.pop queue in
        if t <= q then begin
            printf "%s %d\n" n (ct + t);
            iter (ct + t)
          end
        else begin
            Queue.push (n, t - q) queue;
            iter (ct + q)
          end
      end
  in
  init_q n;
  iter 0
