open Printf
open Scanf

let () =
  let n, q = scanf "%d %d " (fun x y -> (x, y)) in
  let bl = 300 in
  let ar = Array.make n max_int in
  (* let ar = Array.make n 2147483647 in *)
  let blk = Array.make (n / bl + 1) None in
  let wb i = match blk.(i) with
      None -> ()
    | Some v -> let rec iter j =
                  let m = i * bl + j in
                  if j = bl || m = n then blk.(i) <- None
                  else begin
                      ar.(m) <- v;
                      iter (j+1)
                    end in
                iter 0 in
  let find i = match blk.(i / bl) with
      None -> ar.(i)
    | Some v -> v
  in
  let update s t x =
    let rec iter i =
      if i >= t then ()
      else begin
          if i mod bl = 0 && i + bl <= t then begin
              blk.(i / bl) <- Some x;
              iter (i+bl)
            end
          else begin
              begin
                match blk.(i / bl) with
                  None -> ()
                | Some v -> wb (i / bl)
              end;
              ar.(i) <- x;
              iter (i+1)
            end
        end in
    iter s in
  let rec loop i =
    if i = 0 then ()
    else begin
        let com = scanf "%d " (fun x -> x) in
        begin
          if com = 0 then
            let s, t, x = scanf "%d %d %d " (fun x y z -> (x, y+1, z)) in
            update s t x
          else
            let j = scanf "%d " (fun x -> x) in
            find j |> printf "%d\n"
        end;
        loop (i-1)
      end in
  loop q
