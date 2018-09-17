open Str

let read_list f =
  split (regexp " +") (read_line ()) |> List.map f

let () =
  let n = read_int () in
  let am = Array.make_matrix n n 0 in
  let rec read x =
    if x = 0 then
      Array.iter (fun a ->
                  Array.iteri (fun i v ->
                               if i = n-1 then string_of_int a.(i) |> print_endline
                               else (string_of_int a.(i)) ^ " " |> print_string
                              ) a
                 ) am
    else begin
        match read_list int_of_string with
          u :: _ :: vs -> begin
            List.iter (fun v -> am.(u-1).(v-1) <- 1) vs;
            read (x-1)
          end
        | _ -> failwith "read"
      end
  in read n
