fn main(){
  let n = read();
  let mut dv: Vec<usize> = Vec::new();

  for _ in 0 .. n {
    dv.push(read());
  }

  let mut r : usize = 0;

  for i in 0 .. n - 1 {
    if i > r { break; }

    let t = if dv[i] / 10 + i < n { dv[i] / 10 + i } else { n - 1 };
    if t > r { r = t; }
  }

  let mut l : usize = n - 1;
  
  for i in (1 .. n).rev() {
    if i < l { break; }

    let t = if i >= dv[i] / 10 { i - dv[i] / 10 } else { 0 };
    if t < l { l = t; }
  }
  
  println!("{}", if l == 0 && r == n - 1 { "yes" } else { "no" });
}

fn read<T>() -> T
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().parse().unwrap()
}
