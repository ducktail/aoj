use std::cmp;

fn main(){
  loop {
    let pn: Vec<u32> = read_vec();
    let p = pn[0];
    
    if p == 0 { break; }
    
    let vnc:Vec<_> = pn.iter().skip(1).rev().collect();
  
    let vvc:[u32;6] = [500, 100, 50, 10, 5, 1];

    let mut ct: u32 = 20000;
    
    for tp in p .. p+500 {
      if let Some(tc) = pay(tp, &vvc, &vnc) {
        ct = cmp::min(ct,  tc + change(tp - p, &vvc));
      }
    }
    println!("{:?}", ct);
  }
}

fn change(p: u32, vvc: &[u32]) -> u32 {
  let mut ct: u32 = 0;
  let mut x: u32 = p;

  for vc in vvc.iter() {
    let q = x / vc;
    let r = x % vc;
    ct += q;
    x = r;
  }
  ct
}

fn pay(p: u32, vvc: &[u32], vnc: &Vec<&u32>) -> Option<u32> {
  let mut ct: u32 = 0;
  let mut x: u32 = p;

  for (vc, nc) in vvc.iter().zip(vnc.iter()) {
    let q = x / vc;
    ct += cmp::min(**nc, q);
    x -= cmp::min(**nc, q) * vc;
  }
  
  if x == 0 { Some(ct) } else { None }
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
