use std::cmp;

fn main(){
  let q: usize = read();

  for _ in 0 .. q {
    let vcan: Vec<u32> = read_vec();
    let c = vcan[0];
    let a = vcan[1];
    let n = vcan[2];

    let can = cmp::min(c, cmp::min(a, n));
    let c1 = c - can;
    let a1 = a - can;

    let cca = cmp::min(c1 / 2, a1);
    let c2 = c1 - 2 * cca;

    let ccc = c2 / 3;
    
    println!("{}", can + cca + ccc);
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
