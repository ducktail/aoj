fn main(){
  loop {
    let nm: Vec<usize> = read_vec();
    let n = nm[0];
    let m = nm[1];

    if n == 0 && m == 0 { break; }

    let mut gv: Vec<Vec<f64>> = Vec::new();

    for _ in 0 .. n {
      gv.push(read_vec());
    }

    let mut dp: Vec<Vec<f64>> = vec![vec![1.0; n]; m];
    
    for i in 1 .. m {
      for j in 0 .. n {
        let mut mxg: f64 = 0.0;
        for k in 0 .. n {
          let tg = dp[i-1][k] * gv[k][j];
          if tg > mxg { mxg = tg; }
        }
        dp[i][j] = mxg;
      }
    }

    let mut ans: f64 = 0.0;

    for i in 0 .. n {
      if dp[m-1][i] > ans { ans = dp[m-1][i]; }
    }
    
    println!("{:.2}", ans);
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
