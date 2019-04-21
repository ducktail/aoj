fn main(){
  loop {
    let s1: String = read();
    let v1: Vec<char> = s1.chars().collect();
    let v1l = v1.len();

    if v1l == 0 { break; }
    
    let s2: String = read();
    let v2: Vec<char> = s2.chars().collect();
    let v2l = v2.len();

    let mut dp: Vec<Vec<u32>> = vec![vec![0;v2.len()]; v1.len()];

    
    for i in 0 .. v1l {
      if v1[i] == v2[0] {
        dp[i][0] = 1;
      }
    }
    
    for i in 0 .. v2l {
      if v1[0] == v2[i] {
        dp[0][i] = 1;
      }
    }

    for i in 1 .. v1l {
      for j in 1 .. v2l {
        if v1[i] == v2[j] {
          dp[i][j] = dp[i-1][j-1] + 1;
        }
      }
    }

    let ans: &u32 = dp.iter().map(|v| v.iter().max().unwrap()).max().unwrap();
    
    println!("{}", *ans);
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
