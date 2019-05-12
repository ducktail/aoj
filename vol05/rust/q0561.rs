use std::cmp;

fn main(){
  let nk: Vec<usize> = read_vec();
  let n = nk[0];
  let k = nk[1];

  let mut bk: Vec<Vec<usize>> = vec![Vec::new(); 11];

  for _ in 0 .. n {
    let cg: Vec<usize> = read_vec();
    let c = cg[0];
    let g = cg[1];
    bk[g].push(c);
  }

  for i in 1 .. 11 {
    bk[i].sort();
  }

  let mut cv: Vec<Vec<usize>> = vec![vec![0];11];

  for g in 1 .. 11 {
    let mut c = 0;
    let l = bk[g].len();
    
    for i in 1 .. l+1 {
      c += bk[g][l-i];
      cv[g].push(c + i * (i-1));
    }
  }

  let mut dp0: Vec<usize> = vec![0; k + 1];
  
  for i in 0 .. cmp::min(cv[1].len(), k + 1) {
    dp0[i] = cv[1][i];
  }
  
  for g in 2 .. 11 {
    let mut dp1: Vec<usize> = vec![0; k + 1];
    
    for i in 1 .. k + 1 {
      let mut c : usize = 0;
      
      for j in 0 .. cmp::min(cv[g].len(), i + 1) {
        c = cmp::max(c, dp0[i-j] + cv[g][j]);
      }
      
      dp1[i] = c;
    }
    
    dp0 = dp1;
  }
  
  println!("{}", dp0[k]);
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
