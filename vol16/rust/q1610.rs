fn main(){

    const MX: usize = 7368791;
    
    loop {
        let mn: Vec<usize> = read_vec();
        let m = mn[0];
        let n = mn[1];
        
        if m == 0 && n == 0 { break; }

        let mut vb: Vec<bool> = vec![true; MX+1];
        let mut i = m;
        
        for _ in 0 .. n {
            while ! vb[i] { i += 1; }

            let mut j: usize = i * 2;

            while j <= MX { vb[j] = false; j += i; }
            i += 1;
        }

        while ! vb[i] { i += 1; }
        
        println!("{}", i);
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
