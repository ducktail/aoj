use std::cmp;

fn main(){
  let inf: u32 = 2000000000;
  let nm: Vec<usize> = read_vec();
  let n = nm[0];
  let m = nm[1];

  let mut dv: Vec<u32> = Vec::new();

  for _ in 0 .. n {
    dv.push(read());
  }

  let mut dp: Vec<u32> = vec![inf; n+1];
  dp[0] = 0;

  for _ in 0 .. m {
    let c: u32 = read();

    let mut ndp: Vec<u32> = vec![0; n+1];
    
    for i in 1 .. n+1 {
      ndp[i] = cmp::min(dp[i], dp[i-1] + c * dv[i-1]);
    }
    
    dp = ndp;
    
  }
  
  println!("{:?}", dp[n]);
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
