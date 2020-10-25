fn main(){
    loop {
        let nms: Vec<usize> = read_vec();
        let n = nms[0];
        let m = nms[1];
        let s = nms[2];

        if n == 0 && m == 0 && s == 0 { break; }

        let mut dp: Vec<Vec<usize>> = vec![vec![0;s+1];n*n+1];
        dp[0][0] = 1;

        for k in 1 ..= m {
            for i in (0..n*n).rev() {
                for j in (0..s).rev() {
                    if dp[i][j] > 0 && j + k <= s {
                        dp[i+1][j+k] = (dp[i+1][j+k] + dp[i][j]) % 100000;
                    }
                }
            }
        }
        println!("{}",dp[n*n][s])
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
