use std::collections::HashSet;

fn main(){
  let xy = "xy".to_string();
  let yz = "yz".to_string();
  
  loop {
    let nh: Vec<usize> = read_vec();
    let n = nh[0];
    let h = nh[1];

    if n == 0 { break; }

    let mut hs: HashSet<(usize, usize, usize)> = HashSet::new();
    
    for _ in 0 .. h {
      let cab: Vec<String> = read_vec();
      let a: usize = cab[1].parse().unwrap();
      let b: usize = cab[2].parse().unwrap();

      if cab[0] == xy {
        for z in 1 .. n+1 {
          hs.insert((a, b, z));
        }
      } else if cab[0] == yz {
        for x in 1 .. n+1 {
          hs.insert((x, a, b));
        }
      } else {
        for y in 1 .. n+1 {
          hs.insert((a, y, b));
        }
      }
    }
    println!("{}", n * n * n - hs.len());
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
