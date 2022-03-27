use std::cmp;

fn main(){
    let nm: Vec<usize> = read_vec();
    let n = nm[0];
    let m = nm[1];
    let mut graph: Vec<Vec<(usize, u32)>> = vec![vec![];n];
    let mut memo: Vec<Option<u32>> = vec![None;n];

    for _ in 0 .. m {
        let abc: Vec<usize> = read_vec();
        let a = abc[0];
        let b = abc[1];
        let c = abc[2] as u32;
        graph[b].push((a, c));
    }
    
    memo[0] = Some(0);
    
    println!("{:?}", cpath(n-1, &graph, &mut memo));
}

fn cpath(pi: usize, g: &Vec<Vec<(usize, u32)>>, memo: &mut Vec<Option<u32>>) -> u32 {
    if let Some(d) = memo[pi] {
        d
    } else {
        let mut md: u32 = 0;
        for (tpi, td) in &g[pi] {
            md = cmp::max(md, cpath(*tpi, g, memo) + td);
        }
        memo[pi] = Some(md);
        md
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
