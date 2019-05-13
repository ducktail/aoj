fn main(){
  let m: usize = 10000;
  let nk: Vec<usize> = read_vec();
  let n = nk[0];
  let k = nk[1];

  let mut pv: Vec<usize> = vec![0; n + 1];
  
  for _ in 0 .. k {
    let ab: Vec<usize> = read_vec();
    let a = ab[0];
    let b = ab[1];

    pv[a] = b;
  }

  let mut dp: Vec<Vec<usize>> = vec![vec![0; 6]; n + 1];

  match pv[1] {
    0 => {
      dp[1][0] = 1;
      dp[1][2] = 1;
      dp[1][4] = 1;
    },
    1 => {
      dp[1][0] = 1;
    },
    2 => {
      dp[1][2] = 1;
    },
    _ => {
      dp[1][4] = 1;
    },
  }

  for i in 2 .. n + 1 {
    match pv[i] {
      0 => {
        dp[i][0] = (dp[i-1][2] + dp[i-1][3] + dp[i-1][4] + dp[i-1][5]) % m;
        dp[i][1] = dp[i-1][0];
        dp[i][2] = (dp[i-1][0] + dp[i-1][1] + dp[i-1][4] + dp[i-1][5]) % m;
        dp[i][3] = dp[i-1][2];
        dp[i][4] = (dp[i-1][0] + dp[i-1][1] + dp[i-1][2] + dp[i-1][3]) % m;
        dp[i][5] = dp[i-1][4];
      },
      1 => {
        dp[i][0] = (dp[i-1][2] + dp[i-1][3] + dp[i-1][4] + dp[i-1][5]) % m;
        dp[i][1] = dp[i-1][0];
      },
      2 => {
        dp[i][2] = (dp[i-1][0] + dp[i-1][1] + dp[i-1][4] + dp[i-1][5]) % m;
        dp[i][3] = dp[i-1][2];
      },
      _ => {
        dp[i][4] = (dp[i-1][0] + dp[i-1][1] + dp[i-1][2] + dp[i-1][3]) % m;
        dp[i][5] = dp[i-1][4];
      },
    }
  }

  let mut ans: usize = dp[n].iter().sum();
  ans %= m;
  
  println!("{}", ans);
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
