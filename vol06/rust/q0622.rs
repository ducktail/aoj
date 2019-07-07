use std::cmp;

fn main(){
  let ntq: Vec<usize> = read_vec();
  let n = ntq[0];
  let t = ntq[1] as i64;
  let q = ntq[2];

  let mut va: Vec<i64> = Vec::new();
  let mut vd: Vec<u32> = Vec::new();

  for _ in 0 .. n {
    let t: Vec<i64> = read_vec();
    va.push(t[0]);
    vd.push(t[1] as u32);
  }

  let mut ny: usize = 0;
  let mut em: i64 = std::i64::MIN;

  for i in 0 .. n {
    if vd[i] == 2 {
      if i == ny {
        va[i] = cmp::max(va[i] - t, em);
      } else {
        let x = (va[i] + va[i-1]) / 2;
        if va[i] - t > x {
          va[i] -= t;
          for j in ny .. i {
            va[j] += t;
          }
          em = va[i-1];
        } else {
          va[i] = x;
          va[i-1] = x;
          em = x;
          for j in ny .. i-1 {
            va[j] = cmp::min(x, va[j] + t);
          }
        }
      }
      ny = i + 1;
    }
  }

  for i in ny .. n {
    va[i] += t;
  }

  for _ in 0 .. q {
    let i: usize = read();
    println!("{}", va[i-1]);
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
