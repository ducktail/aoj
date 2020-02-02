fn main(){
  let mut dp: Vec<Vec<usize>> = vec![vec![0;1001]; 10];
  dp[0][0] = 1;

  for i in 0 .. 101 {
    let mut ndp: Vec<Vec<usize>> = vec![vec![0;1001]; 10];
    ndp[0][0] = 1;
    for j in 1 .. 10 {
      for k in 0 .. 1001 {
        if k >= i {
          ndp[j][k] = dp[j][k] + dp[j-1][k-i];
        } else {
          ndp[j][k] = dp[j][k];
        }
      }
    }
    dp = ndp;
  }

  loop {
    let ns: Vec<usize> = read_vec();
    let n = ns[0];
    let s = ns[1];

    if n == 0 && s == 0 { break; }

    println!("{}", dp[n][s]);
  }
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
