open Printf
open Scanf

type point = {id : int; x : int; y : int}
               
let () =
  let n = scanf "%d " (fun x -> x) in
  let ps = Array.init n (fun i -> let a, b = scanf "%d %d " (fun x y -> (x, y)) in {id = i; x = a; y = b}) in
  let rt = Array.make 3000000 None in
  let rec make_rt k ix = function
      [||] -> ()
    | ar -> 
       let l = Array.length ar in
       let m = l / 2 in
       if ix then
         Array.fast_sort (fun pa pb -> compare pa.x pb.x) ar
       else
         Array.fast_sort (fun pa pb -> compare pa.y pb.y) ar;
       rt.(k) <- Some ar.(m);
       make_rt (2*k) (not ix) (Array.sub ar 0 m);
       make_rt (2*k+1) (not ix) (try Array.sub ar (m+1) (l-m-1) with _ -> [||])
  in
  make_rt 1 true ps;
  let q = scanf "%d " (fun x -> x) in
  let rec loop c =
    if c = q then ()
    else
      begin
        let sx, tx, sy, ty = scanf "%d %d %d %d " (fun a b c d -> (a, b, c, d)) in
        let rec iter k ix ls =
          match rt.(k) with
            None -> ls
          | Some p -> let ls1 = if sx <= p.x && p.x <= tx && sy <= p.y && p.y <= ty then p :: ls else ls in
                      let ls2 = if ix then
                                  if sx <= p.x then iter (2*k) (not ix) ls1 else ls1
                                else
                                  if sy <= p.y then iter (2*k) (not ix) ls1 else ls1 in
                      let ls3 = if ix then
                                  if p.x <= tx then iter (2*k+1) (not ix) ls2 else ls2
                                else
                                  if p.y <= ty then iter (2*k+1) (not ix) ls2 else ls2 in
                      ls3 in
        iter 1 true [] |> List.sort (fun u v -> compare u.id v.id) |> List.iter (fun v -> printf "%d\n" v.id);
        printf "\n";
        loop (c+1)
      end
  in
  loop 0
