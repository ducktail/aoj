open Printf
open Scanf

module Unionfind =
  struct
    type t = int array
    let create n : t = Array.make n (-1)
    let rec root (uft : t) i =
      let p = uft.(i) in
      if p < 0 then i
      else
        let rt = root uft p in
        uft.(i) <- rt; rt
    let unite (uft : t) i j =
      let ri = root uft i in
      let rj = root uft j in
      if ri = rj then ()
      else
        let si = uft.(ri) in
        let sj = uft.(rj) in
        if si < sj then
          begin
            uft.(ri) <- si + sj;
            uft.(rj) <- ri
          end
        else
          begin
            uft.(ri) <- rj;
            uft.(rj) <- si + sj
          end
    let same (uft : t) i j =
      root uft i = root uft j
    let size (uft : t) i =
      let rt = root uft i in -uft.(rt)
  end

module L =
  struct
    include List
  
    let rec unfold_right f x =
      match f x with
        None -> []
      | Some (a, b) -> a :: unfold_right f b
  end

let solve n =
  let uf = Unionfind.create (n + 1) in
  let rec loop i =
    if i = n then ()
    else let x = L.fold_left (+) 0 @@ L.unfold_right (fun a -> if a = 0 then None else Some (a mod 10, a / 10)) i in
         if i + x <= n then
           Unionfind.unite uf i (i + x)
         else () ;
         loop (i + 1) in
  loop 1 ;
  Unionfind.size uf n

let () =
  scanf "%d " solve |> printf "%d\n"
