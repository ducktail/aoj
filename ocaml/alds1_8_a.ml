open Num

type btree = Node of node | Nil and
     node = {k : num; l : btree; r : btree}

let preorder f tr =
  let rec iter = function
      Nil -> ()
    | Node n -> f n.k; iter n.l; iter n.r
  in iter tr; print_newline ()
  
let inorder f tr =
  let rec iter = function
      Nil -> ()
    | Node n -> iter n.l; f n.k; iter n.r
  in iter tr; print_newline ()

let insert k tr =
  let rec iter = function
      Nil -> Node {k = k; l = Nil; r = Nil}
    | Node n -> if n.k = k then Node n
                else if n.k < k then Node {n with r = iter n.r}
                else Node {n with l = iter n.l}
  in iter tr
          
let () =
  let n = read_int () in
  let rec read tr x =
    if x = 0 then ()
    else let com = read_line () in
         if com = "print" then
           begin
             inorder (fun i -> print_string @@ " " ^ (string_of_num i)) tr;
             preorder (fun i -> print_string @@ " " ^ (string_of_num i)) tr;
             read tr (x-1)
           end
         else let y =
                let l = String.length com in
                num_of_string (String.sub com 7 (l-7)) in
              read (insert y tr) (x-1)
  in
  read Nil n
