use std::collections::VecDeque;
use std::collections::HashSet;

fn main(){
  loop {
    let wh: Vec<usize> = read_vec();
    let w = wh[0];
    let h = wh[1];
    
    if w == 0 && h == 0 { break; }

    let xys: Vec<usize> = read_vec();
    let sp: (usize, usize) = (xys[0]-1, xys[1]-1);
    let xyg: Vec<usize> = read_vec();
    let gp: (usize, usize) = (xyg[0]-1, xyg[1]-1);
    let n: usize = read();

    let mut fv:Vec<Vec<usize>> = vec![vec![0;w];h];

    for _ in 0 .. n {
      let cdxy: Vec<usize> = read_vec();
      let c = cdxy[0];
      let d = cdxy[1];
      let x = cdxy[2] - 1;
      let y = cdxy[3] - 1;

      if d == 0 {
        fv[y][x] = c;
        fv[y][x+1] = c;
        fv[y][x+2] = c;
        fv[y][x+3] = c;
        fv[y+1][x] = c;
        fv[y+1][x+1] = c;
        fv[y+1][x+2] = c;
        fv[y+1][x+3] = c;
      } else {
        fv[y][x] = c;
        fv[y][x+1] = c;
        fv[y+1][x] = c;
        fv[y+1][x+1] = c;
        fv[y+2][x] = c;
        fv[y+2][x+1] = c;
        fv[y+3][x] = c;
        fv[y+3][x+1] = c;
      }
    }

    let sc: usize = fv[sp.1][sp.0];
    let gc: usize = fv[gp.1][gp.0];

    let mut ans: &str = "NG";
    
    if sc != 0 && gc != 0 && sc == gc {
      let mut que: VecDeque<(usize, usize)> = VecDeque::new();
      let mut hs: HashSet<(usize, usize)> = HashSet::new();
      
      que.push_back(sp);
      hs.insert(sp);
      
      
      loop {
        match que.pop_front() {
          None => { break; },
          Some((bx, by)) => {
            if gp == (bx, by) {
              ans = "OK";
              break;
            }
      
            if bx + 1 < w && fv[by][bx+1] == sc && !hs.contains(&(bx+1, by)) {
              que.push_back((bx+1, by));
              hs.insert((bx+1, by));
            }
            if bx > 0 && fv[by][bx-1] == sc && !hs.contains(&(bx-1, by)) {
              que.push_back((bx-1, by));
              hs.insert((bx-1, by));
            }
            if by + 1 < h && fv[by+1][bx] == sc && !hs.contains(&(bx, by+1)) {
              que.push_back((bx, by+1));
              hs.insert((bx, by+1));
            }
            if by > 0 && fv[by-1][bx] == sc && !hs.contains(&(bx, by-1)) {
              que.push_back((bx, by-1));
              hs.insert((bx, by-1));
            }
          },
        }
      }
    }
    println!("{}", ans);
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

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
