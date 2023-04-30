open Printf
open Scanf

let dist (i0, j0) (i1, j1) =
  abs (i0 - i1) + abs (j0 - j1)

let solve h w ar s =
  let tbl = Hashtbl.create (h * w) in
  let ls = String.length s in
  let rec loop1 i j =
    if i = h then ()
    else if j = w then loop1 (i + 1) 0
    else begin
        if ar.(i).[j] <> '_' then
          Hashtbl.add tbl ar.(i).[j] (i, j)
        else () ;
        loop1 i (j + 1)
      end in
  let rec loop2 l p i =
    if i = ls then l
    else
      let np = Hashtbl.find tbl s.[i] in
      loop2 (l + 1 + dist p np) np (i + 1) in
  loop1 0 0 ; loop2 0 (0, 0) 0

let () =
  let id x = x in
  let rec loop () = 
    let h, w = scanf "%d %d " @@ fun h w -> (h, w) in
    if h = 0 && w == 0 then ()
    else
      begin
        let ar = Array.init h @@ fun _ -> scanf "%s " id in
        let s = scanf "%s " id in
        solve h w ar s |> printf "%d\n" ;
        loop ()
      end
  in loop ()
