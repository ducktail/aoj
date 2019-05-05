fn main(){
  loop {
    let hwn: Vec<usize> = read_vec();
    let h = hwn[0];
    let w = hwn[1];
    let n = hwn[2] as u32;

    if h == 0 && w == 0 && n == 0 { break; }
  
    let mut mp: Vec<Vec<u32>> = Vec::new();
  
    for _ in 0 .. h {
      mp.push(read_vec());
    }

    let mut vc: Vec<Vec<u32>> = vec![vec![0; w]; h];

    vc[0][0] = n - 1;

    for i in 1 .. w {
      if vc[0][i-1] % 2 == 0 {
        vc[0][i] = vc[0][i-1] / 2;
      } if mp[0][i-1] == 0 {
        vc[0][i] = vc[0][i-1] / 2;
      } else {
        vc[0][i] = (vc[0][i-1] + 1) / 2;
      }
    }

    for j in 1 .. h {
      if vc[j-1][0] % 2 == 0 {
        vc[j][0] = vc[j-1][0] / 2;
      } if mp[j-1][0] == 1 {
        vc[j][0] = vc[j-1][0] / 2;
      } else {
        vc[j][0] = (vc[j-1][0] + 1) / 2;
      }
    }
    
    for i in 1 .. w {
      for j in 1 .. h {
        match (vc[j][i-1] % 2, vc[j-1][i] % 2) {
          (0, 0) => {
            vc[j][i] = vc[j][i-1] / 2 + vc[j-1][i] / 2;
          },
          (0, 1) => {
            vc[j][i] = vc[j][i-1] / 2 + if mp[j-1][i] == 1 { vc[j-1][i] / 2 } else { (vc[j-1][i] + 1) / 2 };
          },
          (1, 0) => {
            vc[j][i] = if mp[j][i-1] == 0 { vc[j][i-1] / 2 } else { (vc[j][i-1] + 1) / 2 } + vc[j-1][i] / 2;
          },
          _ => {
            vc[j][i] = if mp[j][i-1] == 0 { vc[j][i-1] / 2 } else { (vc[j][i-1] + 1) / 2 } +
                       if mp[j-1][i] == 1 { vc[j-1][i] / 2 } else { (vc[j-1][i] + 1) / 2 } ;
          },
        }
      }
    }
    
    let mut x: usize = 0;
    let mut y: usize = 0;

    loop {
      if x == w || y == h {
        println!("{} {}", y + 1, x + 1);
        break;
      }

      if vc[y][x] % 2 == 0 && mp[y][x] == 0 || vc[y][x] % 2 == 1 && mp[y][x] == 1 {
        y += 1;
      } else {
        x += 1;
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
