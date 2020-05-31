open Printf
open Scanf

let id x = x

let solve m n sg =
  let am = Array.make_matrix (n + 2) (n + 2) false in
  let que = Queue.create () in
  let ars = Array.make (n + 2) false in
  let arg = Array.make (n + 2) false in
  for i = 0 to n do
    for j = 1 to m do
      let k = if i + j >= n + 1 then n + 1 else i + j + sg.(i + j) in
      am.(i).(min (max 0 k) (n + 1)) <- true
    done
  done;
  Queue.push 0 que;
  ars.(0) <- true;
  while (not (Queue.is_empty que)) do
    let i = Queue.pop que in
    for j = 0 to n + 1 do
      if am.(i).(j) then
        begin
          if ars.(j) then ()
          else begin
              ars.(j) <- true;
              Queue.push j que
            end
        end
      else ()
    done
  done;
  Queue.push (n + 1) que;
  arg.(n + 1) <- true;
  while (not (Queue.is_empty que)) do
    let i = Queue.pop que in
    for j = 0 to n + 1 do
      if am.(j).(i) then
        begin
          if arg.(j) then ()
          else begin
              arg.(j) <- true;
              Queue.push j que
            end
        end
      else ()
    done
  done;
  let rec iter i =
    if i = n + 2 then printf "OK\n"
    else if ars.(i) && not arg.(i) then
      printf "NG\n"
    else iter (i + 1) in
  iter 0
    
let () = 
  let rec loop () = 
    let m = scanf "%d " id in
    if m = 0 then ()
    else
      begin
        let n = scanf "%d " id in
        let sg = Array.make (n + 2) 0 in
        for i = 1 to n do
          let d = scanf "%d " id in
          sg.(i) <- d
        done;
        solve m n sg;
        loop ()
      end
  in loop ()
