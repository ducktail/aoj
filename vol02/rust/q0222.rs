fn main(){
  let mut sv: Vec<bool> = vec![true;10000001];

  for i in 2 .. 3163 {
    if sv[i] {
      let mut j = i * i;
      while j < 10000001 {
        sv[j] = false;
        j += i;
      }
    }
  }

  loop {
    let n: usize = read();

    if n == 0 { break; }

    for i in (13 .. n+1).rev() {
      if sv[i] && sv[i-2] && sv[i-6] && sv[i-8] {
        println!("{}", i);
        break;
      }
    }
    
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
