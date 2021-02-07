use std::cmp::min;

fn main(){
    let mut v1: Vec<u32> = (0..1000000).collect();
    let mut v2: Vec<u32> = (0..1000000).collect();

    let ptn: Vec<usize> = (2..).map(|x| x * (x + 1) * (x + 2) / 6).take_while (|x| x < &1000000).collect();

    for &i in ptn.iter() {
        for j in i .. 1000000 {
            v1[j] = min(v1[j], v1[j - i] + 1);
            if i % 2 == 1 {
                v2[j] = min(v2[j], v2[j - i] + 1);
            }
        }
    }
    
    loop {
        let n: usize = read();

        if n == 0 { break; }

        println!("{} {}", v1[n], v2[n]);
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
