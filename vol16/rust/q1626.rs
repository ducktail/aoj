fn main(){
    loop {
        let b: usize = read();

        if b == 0 { break; }

        let mut p: usize = 0;
        let mut l: usize = 0;
        let mut i: usize = 1;

        while i * i <= 2 * b {
            if 2 * b % i == 0 && (2 * b / i - i + 1) % 2 == 0 {
                p = (2 * b / i - i + 1) / 2;
                l = i;
            }
            i += 1;
        }

        println!("{} {}", p, l);
    }
}

#[allow(dead_code)]
fn read<T>() -> T
where T: std::str::FromStr,
      T::Err: std::fmt::Debug
{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).expect("failed to read");
    buf.trim().parse().unwrap()
}

#[allow(dead_code)]
fn read_vec<T>() -> Vec<T>
where T: std::str::FromStr,
      T::Err: std::fmt::Debug
{
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).expect("failed to read");
    buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
