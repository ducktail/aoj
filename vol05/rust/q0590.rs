fn main(){
  let n: usize = read();
  let mut m: usize = 0;

  for _ in 0 .. n {
    let s: u32 = read();
    let mut x: u32 = 1;
    
    loop {
      if 2 * x * x + 2 * x > s { break; }

      if (s - x) % (2 * x + 1) == 0 {
        m += 1;
        break;
      }
      x += 1;
    }
  }

  println!("{}", n - m);
}

fn read<T>() -> T
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().parse().unwrap()
}
