open Printf
open Scanf

let id x = x

let solve q1 b c1 c2 q2 =
  let rec loop noj =
    if noj = 0 then "NA"
    else let non = (b - noj * c1) / c2 in
         if noj + non >= q1 then sprintf "%d %d" noj non
         else loop (noj - 1)
  in loop @@ min q2 @@ b / c1
  
let () =
  let rec loop () =
    let q1 = scanf "%d " id in
    if q1 = 0 then ()
    else begin
        scanf "%d %d %d %d " @@ fun b c1 c2 q2 -> solve q1 b c1 c2 q2 |> printf "%s\n";
        loop ()
      end
  in loop ()
