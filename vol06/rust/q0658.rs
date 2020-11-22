
fn main(){
    let hw: Vec<usize> = read_vec();
    let h = hw[0];
    let w = hw[1];

    let vvc: Vec<Vec<char>> = (0 .. h).map(|_| {let s: String = read(); s.chars().collect()}).collect();

    let mut vo: Vec<Vec<usize>> = vec![vec![0; w]; h];
    let mut vi: Vec<Vec<usize>> = vec![vec![0; w]; h];

    for j in 0 .. w {
        vi[h-1][j] = if vvc[h-1][j] == 'I' {1} else {0};
    }

    for j in 0 .. w {
        for i in (0 .. h-1).rev() {
            vi[i][j] += if vvc[i][j] == 'I' {vi[i+1][j] + 1} else {vi[i+1][j]};
        }
    }
    
    for i in 0 .. h {
        vo[i][w-1] = if vvc[i][w-1] == 'O' {1} else {0};
    }

    for i in 0 .. h {
        for j in (0 .. w-1).rev() {
            vo[i][j] += if vvc[i][j] == 'O' {vo[i][j+1] + 1} else {vo[i][j+1]};
        }
    }

    let mut ans: usize = 0;
    
    for i in 0 .. h {
        for j in 0 .. w {
            if vvc[i][j] == 'J' {
                ans += vi[i][j] * vo[i][j];
            }
        }
    }
    println!("{}", ans);
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
