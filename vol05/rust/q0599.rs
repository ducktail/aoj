use std::cmp;

fn main(){
  let inf: u32 = 1000000000;
  let mn: Vec<usize> = read_vec();
  let m = mn[0];
  let n = mn[1];

  let mut pv: Vec<u32> = Vec::new();

  for _ in 0 .. m {
    pv.push(read());
  }

  pv.push(inf);
  pv.sort_by(|a, b| b.cmp(a));
  pv[0] = 0;

  for i in 1 .. pv.len() {
    pv[i] += pv[i-1];
  }

  let mut dp: Vec<u32> = vec![inf; m + 1];
  dp[0] = 0;

  for _ in 0 .. n {
    let ce: Vec<usize> = read_vec();
    let c = ce[0];
    let e = ce[1] as u32;

    let mut ndp: Vec<u32> = vec![0; m + 1];

    for i in 1 .. m + 1 {
      let mn = if i >= c { dp[i - c] } else { dp[0] };
      ndp[i] = cmp::min(dp[i], mn + e);
    }
    
    dp = ndp;
  }

  let ans: u32 = (0 .. m + 1).map(|i| if pv[i] >= dp[i] { pv[i] - dp[i]} else { 0 }).max().unwrap();
  
  println!("{}", ans);
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
