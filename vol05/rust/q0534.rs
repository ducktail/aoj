use std::cmp;

fn main(){
  loop {
    let n: usize = read();
    
    if n == 0 { break; }

    let mut cv: Vec<u32> = vec![4];

    for _ in 0 .. n {
      cv.push(read())
    }

    cv.push(5);

    let mut mxn: usize = 0;

    for i in 1 .. n + 1 {
      mxn = cmp::max(mxn,
        cmp::max(chain(&cv, i, 1),
          cmp::max(chain(&cv, i, 2),
            chain(&cv, i, 3)
          )
        )
      );
    }

    println!("{}", n - mxn);
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

fn chain(v: &Vec<u32>, i: usize, c: u32) -> usize {
  let mut p: usize = i - 1;
  let mut q: usize = i + 1;
  let mut n: usize;
  
  while v[p] == c { p -= 1; }
  while v[q] == c { q += 1; }

  if q - p - 1 >= 4 {
    n = q - p - 1;
    
    loop {
      if v[p] == v[q] {
        let tc = v[p];
        while v[p] == tc { p -= 1; }
        while v[q] == tc { q += 1; }
        if q - p - 1 - n >= 4 {
          n = q - p - 1;
        } else {
          break;
        }
      } else {
        break;
      }
    }
  } else {
    n = 0;
  }
  
  n
}
