open Str

let read_list f =
  split (regexp " +") (read_line ()) |> List.map f

let print_array a =
  Array.iteri (fun i x -> if i > 0 then print_string " " else (); print_int x) a; print_newline ()
                                                                                    
let () =
  let n = read_int () in
  let ar = read_list int_of_string |> Array.of_list in
  let cnt = ref 0 in
  let merge a left mid right =
    let n1 = mid - left in
    let n2 = right - mid in
    let lar = Array.init (n1+1) (fun i -> if i = n1 then 1000000001 else a.(left + i)) in
    let rar = Array.init (n2+1) (fun i -> if i = n2 then 1000000001 else a.(mid + i)) in
    let rec loop i j k =
      if k = right then ()
      else begin
          incr cnt;
          if lar.(i) <= rar.(j) then begin a.(k) <- lar.(i); loop (i+1) j (k+1) end
          else begin a.(k) <- rar.(j); loop i (j+1) (k+1) end
        end
    in loop 0 0 left
  in
  let rec merge_sort a left right =
    if left + 1 >= right then ()
    else begin
        let mid = (left + right) / 2 in
        merge_sort a left mid;
        merge_sort a mid right;
        merge a left mid right
      end
  in
  merge_sort ar 0 n;
  print_array ar;
  print_endline @@ string_of_int !cnt
