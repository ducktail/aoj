open Printf
open Scanf

module L =
  struct
    include List
  end

module A =
  struct
    include Array

    let print a =
      for i = 0 to 9 do
        printf "%d %d %d %d %d %d %d %d %d %d\n" a.(i).(0) a.(i).(1) a.(i).(2) a.(i).(3) a.(i).(4) a.(i).(5) a.(i).(6) a.(i).(7) a.(i).(8) a.(i).(9)
      done
  end

let print_pos l =
  L.iter (fun (x, y, z) -> printf "%d %d %d\n" x y z) l

let is_sm a (x, y) =
  1 <= x && x <= 8
  && y <= 7
  && a.(y).(x) > 0
  && a.(y+1).(x-1) > 0
  && a.(y+1).(x) > 0
  && a.(y+1).(x+1) > 0
  && a.(y+2).(x) > 0

let is_md a (x, y) =
  x <= 7
  && y <= 7
  && a.(y).(x) > 0
  && a.(y).(x+1) > 0
  && a.(y).(x+2) > 0
  && a.(y+1).(x) > 0
  && a.(y+1).(x+1) > 0
  && a.(y+1).(x+2) > 0
  && a.(y+2).(x) > 0
  && a.(y+2).(x+1) > 0
  && a.(y+2).(x+2) > 0

let is_lg a (x, y) =
  2 <= x && x <= 7
  && y <= 5
  && a.(y).(x) > 0
  && a.(y+1).(x-1) > 0
  && a.(y+1).(x) > 0
  && a.(y+1).(x+1) > 0
  && a.(y+2).(x-2) > 0
  && a.(y+2).(x-1) > 0
  && a.(y+2).(x) > 0
  && a.(y+2).(x+1) > 0
  && a.(y+2).(x+2) > 0
  && a.(y+3).(x-1) > 0
  && a.(y+3).(x) > 0
  && a.(y+3).(x+1) > 0
  && a.(y+4).(x) > 0

let inc_sm a (x, y) z =
  a.(y).(x) <- a.(y).(x) + z ;
  a.(y+1).(x-1) <- a.(y+1).(x-1) + z ;
  a.(y+1).(x) <- a.(y+1).(x) + z ;
  a.(y+1).(x+1) <- a.(y+1).(x+1) + z ;
  a.(y+2).(x) <- a.(y+2).(x) + z

let inc_md a (x, y) z =
  a.(y).(x) <- a.(y).(x) + z ;
  a.(y).(x+1) <- a.(y).(x+1) + z ;
  a.(y).(x+2) <- a.(y).(x+2) + z ;
  a.(y+1).(x) <- a.(y+1).(x) + z ;
  a.(y+1).(x+1) <- a.(y+1).(x+1) + z ;
  a.(y+1).(x+2) <- a.(y+1).(x+2) + z ;
  a.(y+2).(x) <- a.(y+2).(x) + z ;
  a.(y+2).(x+1) <- a.(y+2).(x+1) + z ;
  a.(y+2).(x+2) <- a.(y+2).(x+2) + z

let inc_lg a (x, y) z =
  a.(y).(x) <- a.(y).(x) + z ;
  a.(y+1).(x-1) <- a.(y+1).(x-1) + z ;
  a.(y+1).(x) <- a.(y+1).(x) + z ;
  a.(y+1).(x+1) <- a.(y+1).(x+1) + z ;
  a.(y+2).(x-2) <- a.(y+2).(x-2) + z ;
  a.(y+2).(x-1) <- a.(y+2).(x-1) + z ;
  a.(y+2).(x) <- a.(y+2).(x) + z ;
  a.(y+2).(x+1) <- a.(y+2).(x+1) + z ;
  a.(y+2).(x+2) <- a.(y+2).(x+2) + z ;
  a.(y+3).(x-1) <- a.(y+3).(x-1) + z ;
  a.(y+3).(x) <- a.(y+3).(x) + z ;
  a.(y+3).(x+1) <- a.(y+3).(x+1) + z ;
  a.(y+4).(x) <- a.(y+4).(x) + z

let solve n a =
  let rec dfs c (x, y) =
    if y = 10 then Some []
    else if x = 10 then dfs c (0, y + 1)
    else if a.(y).(x) = 0 then dfs c (x + 1, y)
    else if c = 0 then None
    else if is_sm a (x, y) then begin
        inc_sm a (x, y) (-1) ;
        match dfs (c - 1) (x, y) with
          Some l -> Some ((x, y + 1, 1) :: l)
        | None -> begin
            inc_sm a (x, y) 1 ;
            if is_md a (x, y) then begin
                inc_md a (x, y) (-1) ;
                match dfs (c - 1) (x, y) with
                  Some l -> Some ((x + 1, y + 1, 2) :: l)
                | None -> begin
                    inc_md a (x, y) 1 ;
                    if is_lg a (x, y) then begin
                        inc_lg a (x, y) (-1) ;
                        match dfs (c - 1) (x, y) with
                          Some l -> Some ((x, y + 2, 3) :: l)
                        | None -> begin
                            inc_lg a (x, y) 1 ;
                            None
                          end
                      end
                    else None
                  end
              end
            else if is_lg a (x, y) then begin
                inc_lg a (x, y) (-1) ;
                match dfs (c - 1) (x, y) with
                  Some l -> Some ((x, y + 2, 3) :: l)
                | None -> begin
                    inc_lg a (x, y) 1 ;
                    None
                  end
              end
            else None
          end
      end
    else if is_md a (x, y) then begin
        inc_md a (x, y) (-1) ;
        match dfs (c - 1) (x, y) with
          Some l -> Some ((x + 1, y + 1, 2) :: l)
        | None -> begin
            inc_md a (x, y) 1 ;
            if is_lg a (x, y) then begin
                inc_lg a (x, y) (-1) ;
                match dfs (c - 1) (x, y) with
                  Some l -> Some ((x, y + 2, 3) :: l)
                | None -> begin
                    inc_lg a (x, y) 1 ;
                    None
                  end
              end
            else None
          end
      end
    else if is_lg a (x, y) then begin
        inc_lg a (x, y) (-1) ;
        match dfs (c - 1) (x, y) with
          Some l -> Some ((x, y + 2, 3) :: l)
        | None -> begin
            inc_lg a (x, y) 1 ;
            None
          end
      end
    else None in
  match dfs n (0, 0) with
    Some l -> l
  | None -> [(123, 456, 789)]

let () =
  let id x = x in
  scanf "%d " (fun n ->
      let a = A.init 10 (fun _ -> A.init 10 (fun _ -> scanf "%d " id)) in
      solve n a) |> print_pos
