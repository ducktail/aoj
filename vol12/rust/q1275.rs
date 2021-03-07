fn main(){
    loop {
        let nkm: Vec<usize> = read_vec();
        let mut n: usize = nkm[0];
        let k: usize = nkm[1];
        let m: usize = nkm[2];

        if n == 0 && k == 0 && m == 0 { break }
        
        let mut v: Vec<usize> = (1..=n).collect();

        let mut i: usize = m - 1;

        while n > 1 {
            let _ = v.remove(i);
            n -= 1;
            i = (i + k - 1) % n;
        }

        println!("{}", v[0]);
    }
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
