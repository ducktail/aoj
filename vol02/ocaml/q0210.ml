open Printf
open Scanf

let id x = x

let read_maze w h =
  let mz = Array.make_matrix h w 4 in
  let np = ref 0 in
  for i = 0 to h - 1 do
    let s = scanf "%s " id in
    for j = 0 to w - 1 do
      match s.[j] with
        '#' -> mz.(i).(j) <- 6
      | 'X' -> mz.(i).(j) <- 5
      | 'E' -> begin mz.(i).(j) <- 0; incr np end
      | 'N' -> begin mz.(i).(j) <- 1; incr np end
      | 'W' -> begin mz.(i).(j) <- 2; incr np end
      | 'S' -> begin mz.(i).(j) <- 3; incr np end
      | _ ->   mz.(i).(j) <- 4
    done
  done; (!np, mz)

let solve w h =
  let (np, mz) = read_maze w h in
  let rec loop cp tm =
    if cp = 0 then string_of_int tm
    else if tm = 180 then "NA"
    else begin
        for i = 0 to h - 1 do
          for j = 0 to w - 1 do
            match mz.(i).(j) with
              0 -> if mz.(i+1).(j) = 5 || mz.(i+1).(j) = 4 then mz.(i).(j) <- 3
                   else if mz.(i).(j+1) = 5 || mz.(i).(j+1) = 4 then mz.(i).(j) <- 0
                   else if mz.(i-1).(j) = 5 || mz.(i-1).(j) = 4 then mz.(i).(j) <- 1
                   else if mz.(i).(j-1) = 5 || mz.(i).(j-1) = 4 then mz.(i).(j) <- 2
                   else ()
            | 1 -> if mz.(i).(j+1) = 5 || mz.(i).(j+1) = 4 then mz.(i).(j) <- 0
                   else if mz.(i-1).(j) = 5 || mz.(i-1).(j) = 4 then mz.(i).(j) <- 1
                   else if mz.(i).(j-1) = 5 || mz.(i).(j-1) = 4 then mz.(i).(j) <- 2
                   else if mz.(i+1).(j) = 5 || mz.(i+1).(j) = 4 then mz.(i).(j) <- 3
                   else ()
            | 2 -> if mz.(i-1).(j) = 5 || mz.(i-1).(j) = 4 then mz.(i).(j) <- 1
                   else if mz.(i).(j-1) = 5 || mz.(i).(j-1) = 4 then mz.(i).(j) <- 2
                   else if mz.(i+1).(j) = 5 || mz.(i+1).(j) = 4 then mz.(i).(j) <- 3
                   else if mz.(i).(j+1) = 5 || mz.(i).(j+1) = 4 then mz.(i).(j) <- 0
                   else ()
            | 3 -> if mz.(i).(j-1) = 5 || mz.(i).(j-1) = 4 then mz.(i).(j) <- 2
                   else if mz.(i+1).(j) = 5 || mz.(i+1).(j) = 4 then mz.(i).(j) <- 3
                   else if mz.(i).(j+1) = 5 || mz.(i).(j+1) = 4 then mz.(i).(j) <- 0
                   else if mz.(i-1).(j) = 5 || mz.(i-1).(j) = 4 then mz.(i).(j) <- 1
                   else ()
            | _ -> ()
          done
        done;
        let tcp = ref 0 in
        for i = 0 to h - 1 do
          for j = 0 to w - 1 do
            match mz.(i).(j) with
              4 -> if mz.(i).(j+1) = 2 then begin mz.(i).(j) <- 12; mz.(i).(j+1) <- 14 end
                   else if mz.(i-1).(j) = 3 then begin mz.(i).(j) <- 13; mz.(i-1).(j) <- 14 end
                   else if mz.(i).(j-1) = 0 then begin mz.(i).(j) <- 10; mz.(i).(j-1) <- 14 end
                   else if mz.(i+1).(j) = 1 then begin mz.(i).(j) <- 11; mz.(i+1).(j) <- 14 end
                   else ()
            | 5 -> if j + 1 < w && mz.(i).(j+1) = 2 then begin mz.(i).(j+1) <- 14; incr tcp end
                   else if i - 1 >= 0 && mz.(i-1).(j) = 3 then begin mz.(i-1).(j) <- 14; incr tcp end
                   else if j - 1 >= 0 && mz.(i).(j-1) = 0 then begin mz.(i).(j-1) <- 14; incr tcp end
                   else if i + 1 < h && mz.(i+1).(j) = 1 then begin mz.(i+1).(j) <- 14; incr tcp end
            | _ -> ()
          done
        done;
        for i = 0 to h - 1 do
          for j = 0 to w - 1 do
            if mz.(i).(j) >= 10 then mz.(i).(j) <- mz.(i).(j) - 10
            else ()
          done
        done;
        loop (cp - !tcp) (tm + 1)
      end in
  loop np 0

let () =
  let rec loop () =
    let (w, h) = scanf "%d %d " @@ fun w h -> (w, h) in
    if w = 0 && h = 0 then ()
    else begin
        solve w h |> printf "%s\n";
        loop ()
      end in
  loop ()
