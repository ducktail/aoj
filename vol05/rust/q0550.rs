use std::cmp;

fn main(){
    let n: usize = read();
    let cc: Vec<usize> = (0..n-1).map(|_| read()).collect();

    let mut dp: Vec<Vec<usize>> = vec![vec![200000000 ; 2] ; n / 2 + 1];

    dp[1][0] = 0;
    dp[0][1] = 0;

    for i in 0 .. n - 1 {
        for j in (1 ..= n / 2).rev() {
            let m = cmp::min(dp[j][1] , dp[j][0] + cc[i]);
            dp[j][1] = m;
            let m = cmp::min(dp[j-1][0] , dp[j-1][1] + cc[i]);
            dp[j][0] = m;
        }
    }
    
    println!("{}", dp[n / 2][0]);
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
