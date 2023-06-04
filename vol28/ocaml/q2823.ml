open Printf
open Scanf

let () =
  let rec solve () =
    scanf "%d %d " (fun n m ->
        if n = 0 && m = 0 then ()
        else begin
            let ar = Array.make (m + 1) 0 in
            let rec loop i =
              if i = n then
                printf "%d\n" (Array.fold_left (+) 0 ar)
              else
                begin
                  scanf "%d %d " (fun d v -> if ar.(d) < v then ar.(d) <- v else ()) ;
                  loop (i + 1)
                end in
            loop 0 ;
            solve ()
          end
      ) in
  solve ()
