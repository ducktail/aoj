fn main(){
  loop {
    let v: Vec<usize> = read_vec();
    let n = v[0];
    let m = v[1];

    if n == 0 { break; }
    
    let mut cst: Vec<Vec<u32>> = vec![vec![std::u32::MAX / 4; n]; n];

    for i in 0 .. n {
      cst[i][i] = 0;
    }
    
    let s = v[2] - 1;
    let g1 = v[3] - 1;
    let g2 = v[4] - 1;

    for _ in 0 .. m {
      let v: Vec<usize> = read_vec();
      cst[v[0]-1][v[1]-1] = v[2] as u32;
    }

    wf(&mut cst);

    let mp: u32 = (0 .. n).map(|i| cst[s][i] + cst[i][g1] + cst[i][g2]).min().unwrap();

    println!("{}", mp);
  }
}

fn wf<T>(v: &mut Vec<Vec<T>>)
  where T: Ord + std::ops::Add<Output=T> + Copy
{
  let n = v.len();
  
  for k in 0 .. n {
    for i in 0 .. n {
      for j in 0 .. n {
        if v[i][j] > v[i][k] + v[k][j] { v[i][j] = v[i][k] + v[k][j]; }
      }
    }
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
