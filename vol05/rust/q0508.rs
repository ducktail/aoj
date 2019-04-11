fn main(){
  loop {
    let n: usize = read();
    
    if n == 0 { break; }
    
    let mut adj: Vec<Vec<usize>> = vec![Vec::new();100];
    let mut vt: Vec<bool> = vec![false;100];
    
    for _ in 0 .. n {
      let ab:Vec<usize> = read_vec();
      adj[ab[0]-1].push(ab[1]-1);
      adj[ab[1]-1].push(ab[0]-1);
    }

    let ans = (0..100).map(|i| dfs(&adj, &mut vt, i)).max().unwrap();
    
    println!("{}", ans);
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

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}

fn dfs(adj: &Vec<Vec<usize>>, vt: &mut Vec<bool>, i: usize) -> usize {
  if vt[i] {
    0
  } else {
    vt[i] = true;
    let mut mxl: usize = 0;
    for &j in &adj[i] {
      let k = dfs(adj, vt, j);
      if mxl < k { mxl = k; }
    }
    vt[i] = false;
    mxl + 1
  }
}
