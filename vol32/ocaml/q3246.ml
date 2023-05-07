open Printf
open Scanf

module U =
  struct
    let id x = x

    let iter si ei f =
      let rec loop i =
        if i = ei then ()
        else begin
            f i ;
            loop (i + 1)
          end
      in loop si

    let repeat n f = iter 0 n f
  end

let search_mx_ind a =
  let l = Array.length a in
  let rec loop i mi mx =
    if i = l then mi
    else if a.(i) > mx then loop (i + 1) i a.(i)
    else loop (i + 1) mi mx in
  loop 0 0 0

let solve n =
  let sy = 1896 in
  let ny = 2021 in
  let mg = Array.make (ny - sy + 1) 0 in
  let mm = Array.make (ny - sy + 1) 0 in
  U.repeat n (fun _ ->
    scanf "%d %s %s " (fun y _ m ->
        mm.(y - sy) <- mm.(y - sy) + 1 ;
        if m = "Gold" then mg.(y - sy) <- mg.(y - sy) + 1 else ()
    )) ;
  (sy + search_mx_ind mg, sy + search_mx_ind mm)

let () =
  let rec loop () = 
    let n = scanf "%d " U.id in
    if n = 0 then ()
    else
      begin
        let (gy, my) = solve n in
        printf "%d %d\n" gy my ;
        loop ()
      end
  in loop ()
