open Printf
open Scanf

let solve n =
  let aos = Array.init n @@ fun _ -> scanf "%s " @@ fun x -> x in
  let aot = Array.init n @@ fun _ -> scanf "%s " @@ fun x -> x in
  let check r i j = let (ii, jj) = r (i, j) in
                    if aos.(ii).[jj] = aot.(i).[j] then 0 else 1 in
  let count r0 r1 r2 r3 ct0 ct1 ct2 ct3 =
    let rec loop ct0 ct1 ct2 ct3 i j =
      if i = n then min ct0 @@ min ct1 @@ min ct2 ct3
      else if j = n then loop ct0 ct1 ct2 ct3 (i + 1) 0
      else loop (ct0 + check r0 i j) (ct1 + check r1 i j)
             (ct2 + check r2 i j) (ct3 + check r3 i j) i (j + 1) in
    loop ct0 ct1 ct2 ct3 0 0 in
  let r0 (i, j) = (i, j) in
  let r1 (i, j) = (j, n - 1 - i) in
  let r2 (i, j) = (n - 1 - i, n - 1 - j) in
  let r3 (i, j) = (n - 1 - j, i) in
  count r0 r1 r2 r3 0 1 2 1

let () =
  scanf "%d " solve |> printf "%d\n"
