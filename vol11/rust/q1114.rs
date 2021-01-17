use std::cmp;

fn main(){
    let mut n: usize = read();
    let mut buf = String::new();

    loop {
        let vp: Vec<Vec<usize>> = (0..5).map(|_| read_vec()).collect();
        let mut mp: usize = 0;

        for i in 0..5 {
            for j in 0..5 {
                for k in (i+1)..6 {
                    for l in (j+1)..6 {
                        if (i..k).all(|x| (j..l).all(|y| vp[x][y] == 1)) {
                            mp = cmp::max(mp, (k - i) * (l - j));
                        }
                    }
                }
            }
        }

        println!("{}", mp);
        
        n -= 1;
        if n == 0 { break }
        std::io::stdin().read_line(&mut buf).expect("failed to read");
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
