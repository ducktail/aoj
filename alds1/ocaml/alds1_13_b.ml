open Printf
open Scanf

let id x = x

type state = {mv : int; sp : int; bd : int array}

let () =
  let goal = [|1;2;3;4;5;6;7;8;0|] in
  let isp = ref 0 in
  let bd = Array.init 9 (fun i -> let v = scanf "%d " id in if v = 0 then isp := i; v) in
  let que = Queue.create () in
  let ht = Hashtbl.create 100 in
  let rec loop () =
    if Queue.is_empty que then failwith "8 puzzle"
    else
      let st = Queue.pop que in
      if st.bd = goal then st.mv
      else begin
          List.iter (fun (dx, dy) ->
                     let x, y = (st.sp/3, st.sp mod 3) in
                     let nx, ny = (dx + x, dy + y) in
                     if 0 <= nx && nx <= 2 && 0 <= ny && ny <= 2 then
                       let nsp = 3 * nx + ny in
                       let nbd = Array.copy st.bd in
                       begin
                         nbd.(st.sp) <- st.bd.(nsp);
                         nbd.(nsp) <- 0;
                         if Hashtbl.mem ht nbd then ()
                         else
                           let nst = {mv = st.mv + 1; sp = nsp; bd = nbd} in Queue.push nst que;
                                                                             Hashtbl.add ht nbd true
                       end
                    ) [(1,0);(0,1);(-1,0);(0,-1)];
          loop ()
        end                  
  in
  Queue.push {mv = 0; sp = !isp; bd = bd} que;
  Hashtbl.add ht bd true;
  loop () |> printf "%d\n"
