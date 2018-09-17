use std::io;
use std::cmp;

fn main() {
   
   loop {
        let mut e = String::new();

        io::stdin().read_line(&mut e)
            .expect("Failed to read");

        let e: i32 = e.trim().parse().unwrap();

        if e == 0 {break;}

        println!("{}", solve(e));
    }
}

fn solve(e: i32) -> i32 {
   let mut m: i32 = 1000000000;

   for z in 0.. {
       let z3: i32 = z * z * z;

       if z3 > e {break;}
       
       for y in 0.. {
           let y2: i32 = y * y;
           let x = e - z3 - y2;
           
           if x < 0 {break;}

           m = cmp::min(m, x + y + z);
       }
   }

   m
}
