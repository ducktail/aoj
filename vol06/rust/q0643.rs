use std::cmp;

fn main(){
  let hw: Vec<usize> = read_vec();
  let h = hw[0];
  let w = hw[1];

  let mut va: Vec<Vec<usize>> = Vec::new();

  for _ in 0 .. h {
    va.push(read_vec());
  }

  let mut ml: usize = 2000000000;

  for x in 0 .. w {
    for y in 0 .. h {
      let mut tl: usize = 0;
      
      for i in 0 .. w {
        for j in 0 .. h {
          tl += cmp::min(if x > i {x-i} else {i-x}, if y > j {y-j} else {j-y}) * va[j][i];
        }
      }

      ml = cmp::min(ml, tl);
    }
  }
  println!("{:?}", ml);
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
