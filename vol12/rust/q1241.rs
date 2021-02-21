const N: usize = 32768;

fn main(){
    let mut dp: Vec<Vec<usize>> = vec![vec![0;5];N];
    dp[0][0] = 1;

    for i in (1..182).map(|x| x * x ) {
        for j in 0..4 {
            for k in 0..N {
                if i + k >= N { break }
                
                if dp[k][j] > 0 {
                    dp[i+k][j+1] += dp[k][j];
                }
            }
        }
    }
    
    loop {
        let n: usize = read();

        if n == 0 { break }

        let ans: usize = dp[n].iter().sum();
        println!("{}", ans);
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
