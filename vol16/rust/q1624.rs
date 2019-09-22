fn main(){
  loop {
    let n: usize = read();

    if n == 0 { break; }
    
    let mut va: Vec<u32> = read_vec();
    va.sort();
    
    let s:u32 = va.iter().sum();
    
    // println!("{:?}", s);

    let mut l: i32 = -1;
    let mut r: i32 = n as i32;

    while l + 1 < r {
      let m: i32 = (l + r) / 2;

      if va[m as usize] * (n as u32) <= s {
        l = m;
      } else {
        r = m;
      }
    }

    println!("{:?}", r);
  }
}

fn read<T>() -> T
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().parse().unwrap()
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
