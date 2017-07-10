open Str

type dllist = {value : int option; mutable prev : dllist ref; mutable next : dllist ref}
       
let read_list () =
  split (regexp " +") (read_line ())
                                                 
let () =
  let n = read_int () in
  let rec tn = {value = None; prev = ref tn; next = ref tn} in
  let insert x =
    let nn = {value = Some x; prev = ref tn; next = ref tn} in
    let nnn = !(tn.next) in
    nn.next <- tn.next;
    nn.prev <- nnn.prev;
    tn.next <- ref nn;
    nnn.prev <- ref nn
  in
  let find x =
    let rec loop n =
      let nn = !(n.next) in
      match nn.value with
        None -> raise Not_found
      | Some y -> if x = y then nn else loop nn in
    loop tn
  in
  let delete x = try
      let n = find x in
      let pn = !(n.prev) in
      let nn = !(n.next) in
      pn.next <- n.next;
      nn.prev <- n.prev
    with
      _ -> ()
  in
  let delete_first () =
    let dn = !(tn.next) in
    let nn = !(dn.next) in
    tn.next <- dn.next;
    nn.prev <- dn.prev
  in
  let delete_last () =
    let dn = !(tn.prev) in
    let pn = !(dn.prev) in
    tn.prev <- dn.prev;
    pn.next <- dn.next
  in
  let print_dll () =
    let rec loop n ls =
      let nn = !(n.next) in
      match nn.value with
        None -> List.rev ls
      | Some x -> loop nn (x :: ls) in
    loop tn [] |> List.map string_of_int |> String.concat " " |> print_endline
  in
  let rec iter x =
    if x = 0 then print_dll ()
    else
      let s = read_list () in
      match s with
        "deleteFirst" :: _  -> delete_first (); iter (x-1)
      | "deleteLast" :: _ -> delete_last (); iter (x-1)
      | "insert" :: n :: _ -> insert (int_of_string n); iter (x-1)
      | "delete" :: n :: _ -> delete (int_of_string n); iter (x-1)
      | _ -> failwith "ddl"
  in iter n
