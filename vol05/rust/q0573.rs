use std::cmp;

fn main(){
  let nts: Vec<usize> = read_vec();
  let n = nts[0];
  let t = nts[1];
  let s = nts[2];
  
  let mut dp: Vec<usize> = vec![0; t + 1];

  for _ in 0 .. n {
    let ab: Vec<usize> = read_vec();
    let a = ab[0];
    let b = ab[1];
    
    for ts in (0 .. t).rev() {
      let te = ts + b;
      
      if te <= t && (ts <= s && te <= s || ts >= s && te >= s) {
        let m = dp[ts] + a;
        dp[te] = cmp::max(dp[te], m);
      }
    }
  }
  
  let &ans = dp.iter().max().unwrap();
  
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
