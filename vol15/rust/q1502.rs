use std::cmp;

fn main(){
  let n: usize = read();

  let mut vh: Vec<usize> = (0..n).map(|_| read()).collect();

  vh.sort();

  let mut ret: usize = 1;
  let mut i: usize = 0;
  
  for stp in 1 .. vh[n-1]+1 {
    while stp > vh[i] { i += 1; }
    ret += cmp::min(stp * 4, n - i);
  }

  println!("{:}", ret);
}

fn read<T>() -> T
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().parse().unwrap()
}
