use std::cmp;

fn main(){
  let inf:usize = 1000000000;
  loop {
    let nm: Vec<usize> = read_vec();
    let n = nm[0];
    let m = nm[1];

    if n == 0 && m == 0 { break; }

    let mut rv: Vec<Vec<usize>> = vec![vec![0;1000];n];

    for i in 0 .. n {
      let kv: Vec<usize> = read_vec();

      for j in 0 .. kv[0] {
        rv[i][kv[j*2+1]-1] = kv[j*2+2];
      }
    }

    let mut dv0: Vec<Vec<usize>> = vec![vec![inf;1000]; m+1];
    
    for i in 0 .. 1000 {
      if rv[0][i] > 0 {
        dv0[m][i] = 0;
      }
    }
    
    let mut dv1 = dv0;
    dv0 = vec![vec![inf;1000]; m+1];
    
    for i in 0 .. 1000 {
      if rv[1][i] > 0 && m > 0 {
        dv0[m-1][i] = 0;
      }
      for j in 0 .. 1000 {
        if rv[0][j] > 0 {
          dv0[m][i] = cmp::min(dv0[m][i], dv1[m][j] + (cmp::max(i, j) - cmp::min(i, j)) * (rv[1][i] + rv[0][j]));
        }
      }
    }

    let mut dv2 = dv1;
    dv1 = dv0;
    dv0 = vec![vec![inf;1000]; m+1];
      
    for k in 2 .. n {
      for i in 0 .. 1000 {
        if rv[k][i] > 0 {
          for j in 0 .. 1000 {
            if rv[k-1][j] > 0 {
              for l in 0 .. m+1 {
                dv0[l][i] = cmp::min(dv0[l][i], dv1[l][j] + (cmp::max(i, j) - cmp::min(i, j)) * (rv[k][i] + rv[k-1][j]));
              }
            }
            if rv[k-2][j] > 0 {
              for l in 0 .. m {
                dv0[l][i] = cmp::min(dv0[l][i], dv2[l+1][j] + (cmp::max(i, j) - cmp::min(i, j)) * (rv[k][i] + rv[k-2][j]))
              }
            }
          }
        }
      }
      
      dv2 = dv1;
      dv1 = dv0;
      dv0 = vec![vec![inf;1000]; m+1];
    }
    
    let mut ans: usize = inf;

    for i in 0 .. m+1 {
      for j in 0 .. 1000 {
        ans = cmp::min(ans, dv1[i][j]);
      }
      if i > 0 {
        for j in 0 .. 1000 {
          ans = cmp::min(ans, dv2[i][j]);
        }
      }
    }
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
