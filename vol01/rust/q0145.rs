use std::cmp;

fn main(){
  let n: usize = read();
  let mut cards: Vec<Vec<usize>> = Vec::new();
  let mut dp: Vec<Vec<usize>> = vec![vec![0; n]; n];
  
  for _ in 0 .. n {
    cards.push(read_vec());
  }

  for d in 1 .. n {
    for i in 0 .. n - d {
      let j = i + d;
      let mut cost: usize = 2147483648;
      for k in i .. j {
        cost = cmp::min(cost, dp[i][k] + dp[k+1][j] + cards[i][0] * cards[k][1] * cards[k+1][0] * cards[j][1]);
      }
      dp[i][j] = cost;
    }
  }
  println!("{}", dp[0][n-1]);
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
