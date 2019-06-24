use std::collections::HashSet;
use std::collections::VecDeque;

fn main(){
  let hw: Vec<usize> = read_vec();
  let h = hw[0];
  let w = hw[1];

  let mut vm: Vec<Vec<u32>> = Vec::new();

  for _ in 0 .. h {
    let s: String = read();
    vm.push(s.chars().map(|c| if c == '.' {0} else {c.to_digit(10).unwrap()}).collect());
  }

  let mut sc: HashSet<(usize, usize)> = HashSet::new();
  
  for i in 1 .. h-1 {
    for j in 1 .. w-1 {
      if vm[i][j] > 0 {
        sc.insert((i, j));
      }
    }
  }
  
  let mut ct = 0;

  loop {
    let mut q: VecDeque<(usize, usize)> = VecDeque::new();
    
    for &(i, j) in sc.iter() {
      if vm[i][j] > 0 {
        let an = [(i+1,j),(i+1, j+1),(i, j+1),(i-1,j+1),(i-1,j),(i-1, j-1),(i,j-1),(i+1,j-1)];
        let cn: u32 = an.iter().map(|&(y, x)| {if vm[y][x] == 0 {1} else {0}}).sum();
        if cn >= vm[i][j] {
          q.push_back((i, j));
        }
      }
    }

    if q.is_empty() { break; }
    
    let mut snc: HashSet<(usize, usize)> = HashSet::new();
    
    for &(i, j) in q.iter() {
      vm[i][j] = 0;

      let an = [(i+1,j),(i+1, j+1),(i, j+1),(i-1,j+1),(i-1,j),(i-1, j-1),(i,j-1),(i+1,j-1)];

      for &(ni, nj) in an.iter() {
        if vm[ni][nj] > 0 {
          snc.insert((ni, nj));
        }
      }
    }

    sc = snc;
    
    ct += 1;
  }

  println!("{}", ct);
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
