open Str

type card = {suit : char; value : int}

let card_of_string s = {suit = s.[0]; value = Char.code s.[1] - Char.code '0'}
let string_of_card c = String.init 2 (fun i -> if i = 0 then c.suit else Char.chr (c.value + Char.code '0'))

let is_stable rl sl =
  if List.stable_sort (fun x y -> compare x.value y.value) rl = sl then "Stable" else "Not stable"
              
let bubble_sort c n =
  let a = Array.of_list c in
  let rec iter i j =
    if i = n then Array.to_list a
    else if j = i then iter (i+1) (n-1)
    else if a.(j).value < a.(j-1).value
    then begin
        let t = a.(j) in
        a.(j) <- a.(j-1);
        a.(j-1) <- t;
        iter i (j-1)
      end
    else iter i (j-1)
  in iter 0 (n-1)
      
let selection_sort c n =
  let a = Array.of_list c in
  let rec iter i j minj =
    if i = n then Array.to_list a
    else if j = n then begin
        let t = a.(i) in
        a.(i) <- a.(minj);
        a.(minj) <- t;
        iter (i+1) (i+1) (i+1)
      end
    else if a.(j).value < a.(minj).value then iter i (j+1) j
    else iter i (j+1) minj
  in iter 0 0 0

let read_list () =
  split (regexp " +") (read_line ())

let print_cards cs = List.map string_of_card cs |> String.concat " " |> print_endline
                                                                          
let () =
  let n = read_int () in
  let cs = List.map card_of_string (read_list ()) in
  let bs = bubble_sort cs n in
  let ss = selection_sort cs n in
  print_cards bs;
  is_stable cs bs |> print_endline;
  print_cards ss;
  is_stable cs ss |> print_endline
