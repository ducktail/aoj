use std::cmp;

fn main(){
  loop {
    let n: usize = read();
    if n == 0 { break; }
    
    let mut rv: Vec<usize> = Vec::new();
    let mut mm: Vec<bool> = vec![false;n];
    
    for x in (1 .. n).map(|e| e * e % n) {
      if !mm[x] {
        rv.push(x);
        mm[x] = true;
      }
    }

    let mut ans: Vec<usize> = vec![0; (n + 1) / 2];

    for i in 0 .. rv.len() {
      for j in 0 .. rv.len() {
        ans[dist(n, rv[i], rv[j])] += 1;
      }
    }

    for i in 1 .. ans.len() {
      println!("{:?}", ans[i]);
    }
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

fn dist(n: usize, x: usize, y: usize) -> usize {
  let t = cmp::max(x, y) - cmp::min(x, y);
  cmp::min(t, n - t)
}
