use std::cmp;

fn main(){
  let mn: Vec<usize> = read_vec();
  let m = mn[0];
  let n = mn[1];

  let mut flg: Vec<Vec<char>> = Vec::new();
  
  for _ in 0 .. m {
   let s: String = read();
   flg.push(s.chars().collect());
  }

  let mut emb: Vec<Vec<char>> = Vec::new();
  
  for _ in 0 .. 2 {
   let s: String = read();
   emb.push(s.chars().collect());
  }

  let mut ct: i32 = 0;

  let mut cv: Vec<(usize, usize, char)> = Vec::new();
  
  for i in 0 .. m-1 {
    for j in 0 .. n-1 {
      match (flg[i][j] == emb[0][0],
             flg[i][j+1] == emb[0][1],
             flg[i+1][j] == emb[1][0],
             flg[i+1][j+1] == emb[1][1]) {
             (true, true, true, true) => { ct += 1; },
             (false, true, true, true) => { cv.push((i, j, emb[0][0])); },
             (true, false, true, true) => { cv.push((i, j+1, emb[0][1])); },
             (true, true, false, true) => { cv.push((i+1, j, emb[1][0])); },
             (true, true, true, false) => { cv.push((i+1, j+1, emb[1][1])); },
             _ => {},
      }
    }
  }

  let mut df: i32 = 0;
  
  for &(i, j, c) in &cv {
    let cc = flg[i][j];
    let x = cnt(&flg, &emb, i, j);
    flg[i][j] = c;
    let y = cnt(&flg, &emb, i, j);
    flg[i][j] = cc;
    df = cmp::max(df, y - x);
  }
  
  println!("{}", ct + df);
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

fn cnt(v: &Vec<Vec<char>>, e: &Vec<Vec<char>>, i: usize, j: usize) -> i32 {
  let m = v.len();
  let n = v[0].len();
  let mut ct: i32 = 0;

  if i > 0 && j > 0 &&
     v[i-1][j-1] == e[0][0] &&
     v[i-1][j] == e[0][1] &&
     v[i][j-1] == e[1][0] &&
     v[i][j] == e[1][1] {
       ct += 1;
     }
  if i > 0 && j < n - 1 &&
     v[i-1][j] == e[0][0] &&
     v[i-1][j+1] == e[0][1] &&
     v[i][j] == e[1][0] &&
     v[i][j+1] == e[1][1] {
       ct += 1;
     }
  if i < m - 1 && j > 0 &&
     v[i][j-1] == e[0][0] &&
     v[i][j] == e[0][1] &&
     v[i+1][j-1] == e[1][0] &&
     v[i+1][j] == e[1][1] {
       ct += 1;
     }
  if i < m - 1 && j < n - 1 &&
     v[i][j] == e[0][0] &&
     v[i][j+1] == e[0][1] &&
     v[i+1][j] == e[1][0] &&
     v[i+1][j+1] == e[1][1] {
       ct += 1;
     }
     
   ct
}
