wopen Printf
open Scanf

type node = { p : int option; l : int option; r : int option }
              
let id x = x

let find_root tr =
  let rec iter i = match tr.(i) with
      {p = Some j} -> iter j
    | _ -> i
  in iter 0

let preorder tr n =
  let rec iter ls = function
      None -> ls
    | Some i -> let ts1 = iter ls tr.(i).r in
                let ts2 = iter ts1 tr.(i).l in i :: ts2
  in iter [] (Some n)

let inorder tr n =
  let rec iter ls = function
      None -> ls
    | Some i -> let ts1 = iter ls tr.(i).r in
                let ts2 = iter (i :: ts1) tr.(i).l in ts2
  in iter [] (Some n)

let postorder tr n =
  let rec iter ls = function
      None -> ls
    | Some i -> let ts1 = iter (i::ls) tr.(i).r in
                let ts2 = iter ts1 tr.(i).l in ts2
  in iter [] (Some n)

let () =
  let n = scanf "%d\n" id in
  let btree = Array.make n {p = None; l = None; r = None} in
  let rec read n =
    if n = 0 then ()
    else begin
        let i, l, r = scanf "%d %d %d\n" (fun x y z -> (x, y, z)) in
        begin
          btree.(i) <- {btree.(i) with l = (if l <> (-1) then Some l else None);
                                       r = (if r <> (-1) then Some r else None)};
          if l <> (-1) then btree.(l) <- {btree.(l) with p = Some i};
          if r <> (-1) then btree.(r) <- {btree.(r) with p = Some i}
        end;
        read (n-1)
      end
  in
  read n;
  let r = find_root btree in
  printf "Preorder\n";
  preorder btree r |> List.iter (fun x -> printf " %d" x); printf "\n";
  printf "Inorder\n";
  inorder btree r |> List.iter (fun x -> printf " %d" x); printf "\n";
  printf "Postorder\n";
  postorder btree r |> List.iter (fun x -> printf " %d" x); printf "\n"
