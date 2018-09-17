open Printf
       
type point = {x:float; y:float}

let add p1 p2 = {x = p1.x +. p2.x; y = p1.y +. p2.y}
let sub p1 p2 = {x = p1.x -. p2.x; y = p1.y -. p2.y}
let mul k p = {x = k *. p.x; y = k *. p.y}
let rot60 v = let sqrt32 = (sqrt 3.0) /. 2.0 in {x = 0.5 *. v.x -. sqrt32 *. v.y; y = sqrt32 *. v.x +. 0.5 *. v.y}
let print_points ps =
  List.iter (fun x -> printf "%.8f %.8f\n" x.x x.y) ps

let () =
  let n = read_int () in
  let koch_curve n =
    let rec loop n p1 p2 ls =
      if n = 0 then p2 :: ls
      else let s = add (mul (2.0 /. 3.0) p1) (mul (1.0 /. 3.0) p2) in
           let t = add (mul (1.0 /. 3.0) p1) (mul (2.0 /. 3.0) p2) in
           let u = add s (rot60 (sub t s)) in
           let l1 = loop (n-1) t p2 ls in
           let l2 = loop (n-1) u t l1 in
           let l3 = loop (n-1) s u l2 in
           let l4 = loop (n-1) p1 s l3 in
           l4
    in
    let p1 = {x = 0.0; y = 0.0} in
    let p2 = {x = 100.0; y = 0.0} in
    p1 :: (loop n p1 p2 [])
  in
  print_points @@ koch_curve n
