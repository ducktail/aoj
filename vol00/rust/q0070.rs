fn main(){
  let mut v: Vec<Vec<u32>> = vec![vec![0;331];11];
  let mut usd: Vec<bool> = vec![false; 10];
  
  comb(&mut v, &mut usd, 1, 0);
  
  loop {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
      Ok(l) => {
        if l == 0 { break; }
        else {
          let t: Vec<usize> = buf.split_whitespace().map(|e| e.parse().unwrap()).collect();
          let n = t[0];
          let s = t[1];

          println!("{}", if s > 330 { 0 } else { v[n][s] });
        }
      },
      Err(_) => { println!("failed to read"); },
    }
  }
}

fn comb(v: &mut Vec<Vec<u32>>, usd: &mut Vec<bool>, i: usize, sm: usize) {
  if i < 11 {
    for k in 0 .. 10 {
      if !usd[k] {
        usd[k] = true;
        let nsm = sm + i * k;
        v[i][nsm] += 1;
        comb(v, usd, i+1, nsm);
        usd[k] = false;
      }
    }
  }
}
