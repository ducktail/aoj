open Printf
open Scanf

let id x = x

let mdst (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

let solve mp lm =
  let rec loop0 sp x d =
    if sp = 6 then (x, d) else
      let np = if sp = 5 then 1 else sp + 1 in
      let ar0 = Array.init lm.(np) @@
                  fun i -> mdst mp.(0).(0) mp.(np).(i) in
      let rec loop1 n p ar =
        if n = 0 then (p, ar) else
          let np = if p = 5 then 1 else p + 1 in
          let nar = Array.init lm.(np) @@
                      fun i -> let rec loop2 j d =
                                 if j = lm.(p) then d
                                 else
                                   loop2 (j + 1) (min d (ar.(j) + mdst mp.(p).(j) mp.(np).(i))) in
                               loop2 0 100000 in
          loop1 (n - 1) np nar in
      let (p1, ar1) = loop1 3 np ar0 in
      let d1 = let rec loop3 i d =
                 if i = lm.(p1) then d
                 else loop3 (i + 1) (min d (ar1.(i) + mdst mp.(p1).(i) mp.(6).(0)))
               in loop3 0 100000 in
      loop0 (sp + 1) (if d1 < d then sp else if d1 = d && sp < x then sp else x) (min d d1) in
  let (mx, md) = loop0 1 0 100000 in
  if md >= 100000 then "NA" else sprintf "%d %d" mx md
        
let () =
  let rec loop () =
    let w, h = scanf "%d %d " @@ fun w h -> (w, h) in
    if w = 0 && h = 0 then ()
    else begin
        let mp = Array.make_matrix 7 1000 (-1, -1) in
        let lm = Array.make 7 0 in
        for i = 0 to h - 1 do
          let s = scanf "%s " id in
          for j = 0 to w - 1 do
            if s.[j] = 'S' then
              mp.(0).(0) <- (i, j)
            else if s.[j] = 'G' then
              mp.(6).(0) <- (i, j)
            else if s.[j] = '.' then ()
            else begin
                let atr = Char.code s.[j] - Char.code '0' in
                mp.(atr).(lm.(atr)) <- (i, j);
                lm.(atr) <- lm.(atr) + 1
              end
          done
        done;
        solve mp lm |> printf "%s\n";
        loop ()
      end
  in loop ()
