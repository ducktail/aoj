let insertion_sort a n g =
  let rec iloop i c =
    if i = n then c
    else let v = a.(i) in
         let rec jloop j c' =
           if j < 0 || a.(j) <= v then begin
               a.(j+g) <- v;
               iloop (i+1) (c+c')
             end
           else begin
               a.(j+g) <- a.(j);
               jloop (j-g) (c'+1)
             end
         in jloop (i-g) 0
  in iloop g 0

let shell_sort a n =
  let rec initg l =
    let h = List.hd l in
    let nh = 3 * h + 1 in
    if nh >= n then l
    else initg (nh::l) 
  in
  let gl = initg [1] in
  let m = List.length gl in
  let rec iter c = function
      [] -> c
    | g :: gs -> let d = insertion_sort a n g in iter (c+d) gs in
  let cnt = iter 0 gl in
  (m, gl, cnt)
        
let () =
  let n = read_int () in
  let ar = Array.init n (fun _ -> read_int ()) in
  let (m, gl, cnt) = shell_sort ar n in
  string_of_int m |> print_endline;
  List.map string_of_int gl |> String.concat " " |> print_endline;
  string_of_int cnt |> print_endline;
  Array.iter (fun v -> string_of_int v |> print_endline) ar
