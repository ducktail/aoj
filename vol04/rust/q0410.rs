fn main(){
    let n: usize = read();
    let vp: Vec<usize> = read_vec();

    let mut cnt: usize = 0;
    
    for st in 1 .. (1 << n) {
        let cp: usize = (0..n).fold(1, |x, i| if st & (1 << i) == 0 { x } else { lcm(vp[i], x)});
        if (0..n).filter(|&i| st & (1 << i) == 0).all(|i| cp % vp[i] != 0) { cnt += 1 }
    }

    println!("{}", cnt)
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 { a }
    else { gcd(b, a % b)}
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
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
