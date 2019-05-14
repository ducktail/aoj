use std::collections::VecDeque;

fn main(){
  let wh: Vec<usize> = read_vec();
  let w = wh[0];
  let h = wh[1];

  let mut mp: Vec<Vec<usize>> = vec![vec![0; h+2]; w+2];

  for y in 1 .. h + 1 {
    let tv: Vec<usize> = read_vec();
    
    for x in 1 .. w + 1 {
      mp[x][y] = tv[x-1];
    }
  }

  let np: Vec<Vec<(i32, i32)>> = vec![
    vec![(1, 0), (-1, 0), (-1, -1), (0, -1), (-1, 1), (0, 1)],
    vec![(1, 0), (-1, 0), (0, -1), (1, -1), (0, 1), (1, 1)]
  ];
  
  let mut que: VecDeque<(usize, usize)> = VecDeque::new();

  que.push_back((0, 0));
  
  mp[0][0] = 2;
  
  loop {
    match que.pop_front() {
      Some((x, y)) => {
        for &(dx, dy) in &np[y % 2] {
          let nx = x as i32 + dx;
          let ny = y as i32 + dy;

          if nx >= 0 &&
             nx <= w as i32 + 1 &&
             ny >= 0 &&
             ny <= h as i32 + 1 &&
             mp[nx as usize][ny as usize] == 0 {
            mp[nx as usize][ny as usize] = 2;
            que.push_back((nx as usize, ny as usize));
          }
        }
      },
      None => { break; },
    }
  }

  let mut ans: usize = 0;

  for y in 1 .. h + 1 {
    for x in 1 .. w + 1 {
      if mp[x][y] == 1 {
        for &(dx, dy) in &np[y % 2] {
          let nx = (x as i32 + dx) as usize;
          let ny = (y as i32 + dy) as usize;
          
          if mp[nx][ny] == 2 {
            ans += 1;
          }
        }
      }
    }
  }
  
  println!("{}", ans);
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
