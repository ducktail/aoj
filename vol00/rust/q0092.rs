use std::cmp;

fn main(){
  loop {
    let n: usize = read();
    if n == 0 {break;}
    
    let mut cv: Vec<Vec<char>> = vec![vec!['.'; n]; n];
    let mut lv: Vec<Vec<u32>> = vec![vec![0; n+1]; n+1];
    let mut mxl: u32 = 0;
    
    for i in 0 .. n {
      cv[i] = read_charvec();
    }
  
    for i in 0..n {
      for j in 0..n {
        if cv[i][j] == '*' {
          lv[i+1][j+1] = 0;
        } else {
          let x: u32 = cmp::min(lv[i][j], cmp::min(lv[i][j+1], lv[i+1][j]));
          lv[i+1][j+1] = x+1;
          mxl = cmp::max(mxl, x+1);
        }
      }
    }
    println!("{}", mxl);
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

fn read_charvec() -> Vec<char>
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().chars().collect()
}
