open Str

let read_list f =
  split (regexp " +") (read_line ()) |> List.map f

let print_array a = Array.to_list a |> List.map string_of_int |> String.concat " " |> print_endline

let bubble_sort a n =
  let rec oloop i c fin =
    if fin then c
    else let rec iloop j c' fin' =
           if j = i then oloop (i+1) c' fin'
           else if a.(j) < a.(j-1) then begin
               let v = a.(j) in
               a.(j) <- a.(j-1);
               a.(j-1) <- v;
               iloop (j-1) (c'+1) false
             end
           else iloop (j-1) c' fin' in
         iloop (n-1) c true
  in
  oloop 0 0 false
                        
let () =
  let n = read_int () in
  let ar = Array.of_list (read_list int_of_string) in
  let c = bubble_sort ar n in
  print_array ar; string_of_int c |> print_endline
