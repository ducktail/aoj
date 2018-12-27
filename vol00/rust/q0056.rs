fn main(){
  let mxv: usize = 50000;
  let mxi: usize = 224;
  let mut tbl: Vec<bool> = vec![true; mxv];

  tbl[0] = false;
  tbl[1] = false;

  for i in 2 .. mxi {
    if tbl[i] {
      let mut j = i * i;
      while j < mxv {
        tbl[j] = false;
        j += i;
      }
    }
  }

  loop {
    let mut cnt: u32 = 0;
    let n: usize = read();

    if n == 0 { break; }

    for i in 2 .. n / 2 + 1 {
      if tbl[i] && tbl[n-i] {cnt += 1;}
    }

    println!("{}", cnt);
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
