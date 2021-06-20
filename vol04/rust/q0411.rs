fn main(){
    let nl: Vec<usize> = read_vec();
    let n = nl[0];
    let l = nl[1];

    let mut vpd: Vec<Vec<usize>> = (0..n).map(|_| read_vec()).collect();
    vpd.sort();

    let mut stack: Vec<(usize, usize)> = Vec::new();
    let mut pt: usize = 0;
    
    for i in 0 .. n {
        let p = vpd[i][0];
        let d = vpd[i][1];
        if d == 1 {
            stack.push((i+1, p))
        } else {
            if stack.is_empty() {
                pt += p - i - 1
            } else {
                if let Some((si, sp)) = stack.pop() {
                    pt += p - sp -i - 1 + si;
                }
            }
        }
    }

    while !stack.is_empty() {
        if let Some((i, p)) = stack.pop() {
            pt += l - p - n + i;
        }
    }
    println!("{}", pt)
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
