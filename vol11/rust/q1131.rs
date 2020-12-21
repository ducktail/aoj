fn main(){
    loop {
        let vt: Vec<usize> = read_vec();
        let p = vt[0];
        let q = vt[1];
        let a = vt[2];
        let n = vt[3];

        if p == 0 && q == 0 && a == 0 && n == 0 { break }
        let ans: usize = solve(p, q, a, n, 0, 1, 0, 1);
        println!("{:?}", ans)
    }
}

fn solve(p: usize, q: usize, a: usize, n: usize, nm: usize, dn: usize, ct: usize, md: usize) -> usize {
    if p * dn == q * nm { 1 }
    else if ct == n { 0 }
    else {
        let mut c: usize = 0;
        let mut d: usize = md;

        while d * dn <= a {
            c += solve(p, q, a, n, nm * d + dn, dn * d, ct + 1, d);
            d += 1;
        }
        c
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
