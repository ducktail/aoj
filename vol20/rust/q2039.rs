use std::io;

fn main(){
   let sv = sieve(30000);

   loop {
     let n = read_i32();
     if n == 0 {break;}

     let mut cnt: i32 = 0;

     if n % 2 == 0 {
       if n == 6 {cnt += 1;}

       let mut x: i32 = 3;

       while x <= n - 2 - x {
         if is_pn(&sv, x) && is_pn(&sv, n - 2 - x) && is_triangle(2, x, n - 2 - x) {
           cnt += 1;
         }
         x += 2;
       }
     } else {
       if n == 7 {cnt += 1;}

       let mut x: i32 = 3;

       while x <= n - 2 * x {
         let mut y: i32 = x;

         while y <= n - x - y {
           if is_pn(&sv, x) && is_pn(&sv, y) && is_pn(&sv, n - x - y) && is_triangle(x, y, n - x - y) {
             cnt += 1;
           }
           y += 2;
         }
         x += 2;
       }
     }

     println!("{}", cnt);
   }
}

fn is_pn(v: &Vec<bool>, x: i32) -> bool{
  if x < 0 {
    false
  } else {
    v[x as usize]
  }
}

fn sieve(n: usize) -> Vec<bool> {
  let mut v = vec! [true; n + 1];

  v[0] = false;
  v[1] = false;

  let mut i = 2;

  while i * i <= n {
    if v[i] {
      let mut j = i * i;
      while j <= n {
        v[j] = false;
        j += i;
      }
    }
    i += 1;
  }

  v
}

fn read_i32() -> i32{
  let mut buf = String::new();

  io::stdin().read_line(&mut buf).expect("failed to read");

  buf.trim().parse().unwrap()
}

fn is_triangle(x: i32, y: i32, z: i32) -> bool{
 let mut v: Vec<i32> = vec![x, y, z];
 v.sort();
 v[0] + v[1] > v[2]
}
