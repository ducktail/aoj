fn main(){
  loop {
    let n: usize = read();

    if n == 0 { break; }

    let mut vsc: Vec<Vec<usize>> = vec![vec![];31];

    for i in 0 .. n {
      let vt: Vec<usize> = read_vec();

      for j in 1 .. vt.len() {
        vsc[vt[j]].push(i);
      }
    }

    let ans: i32 = solve(n, &vsc);
    println!("{}", ans);
  }
}

fn solve(n: usize, vsc: &Vec<Vec<usize>>) -> i32 {
  let mut vst: Vec<u64> = (0 .. n).map(|i| 1 << i).collect();
  let gat: u64 = 2u64.pow(n as u32) - 1;

  let mut d: usize = 1;
  let ret: i32;
  
  loop {
    if d == 31 { ret = -1; break; }

    let mut t: u64 = 0;
    
    for s in &vsc[d] {
      t |= vst[*s];
    }

    for s in &vsc[d] {
      vst[*s] = t;
    }
    
    if (0..n).any(|i| vst[i] == gat) { ret = d as i32; break; }
    
    d += 1;
  }
  
  ret
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
