fn main(){
  
  let pn = sieve(1000000);
  
  loop {
    let n: usize = read();
    if n == 0 { break; }

    let mut ans = 0;
    let mut i = 3;

    while 2 * i <= n {
      if pn[i] && pn[n-i] {
        ans += 1;
      }
      i += 2;
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

fn sieve(n: usize) -> Vec<bool> {
  let mut v: Vec<bool> = vec![true; n+1];
  v[0] = false;
  v[1] = false;

  let mut i = 2;

  while i * i <= n {
    if v[i] {
      let mut j = i * i;
      while j <= n {
        v[j] = false;
        j += i;
      }
    }
    i += 1;
  }
  
  v
}
