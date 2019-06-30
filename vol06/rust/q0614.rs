use std::cmp;

fn main(){
  let nm: Vec<usize> = read_vec();
  let n = nm[0];
  let m = nm[1];

  let vp: Vec<usize> = read_vec();

  let mut vr: Vec<i64> = vec![0;n];
  
  for i in 0 .. m-1 {
    if vp[i] < vp[i+1] {
      vr[vp[i]-1] += 1;
      vr[vp[i+1]-1] -= 1;
    } else {
      vr[vp[i+1]-1] += 1;
      vr[vp[i]-1] -= 1;
    }
  }
  
  // println!("{:?}", vr);

  let mut ct: i64 = 0;
  let mut ans: i64 = 0;
  
  for i in 0 .. n-1 {
    let abc: Vec<i64> = read_vec();
    let a = abc[0];
    let b = abc[1];
    let c = abc[2];

    ct += vr[i];

    ans += cmp::min(ct * a, ct * b + c);
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
