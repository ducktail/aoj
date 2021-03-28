use std::cmp;

fn main(){
    loop {
        let n: usize = read();

        if n == 0 { break; }

        let vw: Vec<usize> = read_vec();

        let mut dp: Vec<Vec<usize>> = vec![vec![0 ; n+1] ; n];

        for l in 2 ..= n {
            for i in 0 ..= n - l {
                for j in i+1 .. i+l {
                    dp[i][i+l] = cmp::max(dp[i][i+l], dp[i][j] + dp[j][i+l]);
                }
                if dp[i+1][i+l-1] == l - 2 {
                    dp[i][i+l] = cmp::max(dp[i][i+l], l - 2 + if balance(&vw[i], &vw[i+l-1]) {2} else {0});
                }
            }
        }
        println!("{}", dp[0][n]);
    }
}

fn balance(x: &usize, y: &usize) -> bool {
    ((*x as isize) - (*y as isize)).abs() <= 1
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
