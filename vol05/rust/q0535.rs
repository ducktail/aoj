use std::cmp;

fn main(){
  loop {
    let m:usize = read();
    let n:usize = read();

    if m == 0 && n == 0 { break; }

    let mut iv: Vec<Vec<u32>> = vec![vec![0;m+2]; n+2];

    for i in 1 .. n+1 {
      let tv: Vec<u32> = read_vec();
      for j in 0 .. m {
        iv[i][j+1] = tv[j];
      }
    }

    let mut ans: u32 = 0;

    for i in 1 .. n+1 {
      for j in 1 .. m+1 {
        ans = cmp::max(ans, dfs(&mut iv, i, j));
      }
    }

    println!("{}", ans);
    // println!("{:?}, {:?}, {:?}, {:?}, {:?}", dfs(&mut iv, 1, 1), 1, 11, 111, 1111);
  }
  // let x: u32 = read();
  // let y: i64 = read();
  // let z: f64 = read();
  // let a: String = read();
  // let b: Vec<u32> = read_vec();
  // println!("{:?}, {:?}, {:?}, {:?}, {:?}", x, y, z, a, b);
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

fn dfs(v: &mut Vec<Vec<u32>>, h: usize, w: usize) -> u32 {
  if v[h][w] == 0 {
    0
  } else {
    v[h][w] = 0;
    
    let l0 = dfs(v, h+1, w);
    let l1 = dfs(v, h, w+1);
    let l2 = dfs(v, h-1, w);
    let l3 = dfs(v, h, w-1);

    v[h][w] = 1;

    1 + cmp::max(
          cmp::max(l0, l1),
          cmp::max(l2, l3))
  }
}
