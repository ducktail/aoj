use std::cmp;

fn main(){
  loop {
    let rc: Vec<usize> = read_vec();
    let r = rc[0];
    let c = rc[1];
    
    if r == 0 && c == 0 { break; }
  
    let mut av: Vec<usize> = vec![0;c];
    let mut mxs: usize = 0;
  
    for _ in 0 .. r {
      let tv: Vec<usize> = read_vec();
      for i in 0 .. c {
        av[i] = av[i] * 2 + tv[i];
      }
    }

    for i in 0 .. 2usize.pow(r as u32) {
      let x: usize = av.iter().map(|t| (t ^ i).count_ones() as usize)
      .map(|t| cmp::max(t, r-t)).sum();
      mxs = cmp::max(mxs, x);
    }
    
    println!("{}", mxs);
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
