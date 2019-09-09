fn main(){
  let cs: Vec<u32> = (2..18).map(|i| i * i).collect();

  let mut dp: Vec<u32> = vec![1;301];

  for c in cs {
    let mut ndp: Vec<u32> = vec![0;301];
    for i in 0 .. 301 {
      let ii = i as u32;
      if ii < c {
        ndp[i] = dp[i];
      } else {
        let j = (ii - c) as usize;
        ndp[i] = dp[i] + ndp[j];
      }
    }
    dp = ndp;
  }

  loop {
    let n: usize = read();
    
    if n == 0 { break; }

    println!("{}", dp[n]);
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
