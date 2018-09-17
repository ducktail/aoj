open Printf
open Str

type node = {p : int option; cs : int list; d : int}
              
let read_list f =
  split (regexp " +") (read_line ()) |> List.map f

let string_of_list ls =
  let s = List.map string_of_int ls |> String.concat ", " in "[" ^ s ^ "]"

let int_of_parent n = match n.p with
    None -> (-1)
  | Some x -> x

let type_of_node = function
    {p = None} -> "root"
  | {cs = []} -> "leaf"
  | _ -> "internal node"

let find_root tr =
  let n = Array.length tr in
  let rec iter i =
    if i = n then failwith "find_root"
    else match tr.(i) with
           {p = None} -> i
         | _ -> iter (i+1)
  in iter 0

let set_depth tr r =
  let rec iter i d =
    let n = tr.(i) in tr.(i) <- {n with d = d};
                      match n.cs with
                        [] -> ()
                      | l -> List.iter (fun x -> iter x (d+1)) l
  in iter r 0
  
let () =
  let n = read_int () in
  let tree = Array.make n {p = None; cs = []; d = 0} in
  let rec read n =
    if n = 0 then ()
    else begin
        begin
          match read_list int_of_string with
            id::_::rs -> begin
              tree.(id) <- begin let n = tree.(id) in {n with cs = rs} end;
              List.iter (fun x -> let n = tree.(x) in tree.(x) <- {n with p = Some id}) rs
            end
          | _ -> failwith "read"
        end;
        read (n-1)
      end
  in
  read n;
  begin let r = find_root tree in set_depth tree r end;
  Array.iteri (fun i x -> let pi = int_of_parent x in
                          let sl = string_of_list x.cs in
                          let tn = type_of_node x in
                          let dn = x.d in
                          printf "node %d: parent = %d, depth = %d, %s, %s\n" i pi dn tn sl) tree
