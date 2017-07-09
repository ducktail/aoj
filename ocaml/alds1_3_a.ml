open Str

let read_list () =
  split (regexp " +") (read_line ())
                                                 
let () =
  let com = read_list () in
  let rec iter stack = function
      [] -> List.hd stack
    | "+" :: rs -> begin
        match stack with
          x :: y :: ys -> iter ((y + x) :: ys) rs
        | _ -> failwith "stack"
      end
    | "-" :: rs -> begin
        match stack with
          x :: y :: ys -> iter ((y - x) :: ys) rs
        | _ -> failwith "stack"
      end
    | "*" :: rs -> begin
        match stack with
          x :: y :: ys -> iter ((y * x) :: ys) rs
        | _ -> failwith "stack"
      end
    | n :: rs -> iter ((int_of_string n) :: stack) rs
  in
  iter [] com |> string_of_int |> print_endline
