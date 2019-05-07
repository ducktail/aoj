fn main(){
  let m: u32 = 100000;
  
  loop {
    let wh: Vec<usize> = read_vec();
    let w = wh[0];
    let h = wh[1];

    if w == 0 && h == 0 { break; }

    let mut dp: Vec<Vec<Vec<u32>>> = vec![vec![vec![0;4];w];h];

    for i in 1 .. w {
      dp[0][i][0] = 1;
    }

    for j in 1 .. h {
      dp[j][0][3] = 1;
    }

    for j in 1 .. h {
      for i in 1 .. w {
        dp[j][i][0] = (dp[j][i-1][0] + dp[j][i-1][1]) % m;
        dp[j][i][1] = dp[j][i-1][3];
        dp[j][i][2] = dp[j-1][i][0];
        dp[j][i][3] = (dp[j-1][i][2] + dp[j-1][i][3]) % m;
      }
    }

    let mut ans: u32 = dp[h-1][w-1].iter().sum();
    ans %= m;
    
    println!("{}", ans);
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
