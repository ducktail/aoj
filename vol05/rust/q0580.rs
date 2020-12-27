fn main(){
    let nk: Vec<usize> = read_vec();
    let n = nk[0];
    let k = nk[1];
    let va: Vec<Vec<u64>> = (0..n).map(|_| read_vec()).collect();

    let mut vx: Vec<u64> = va.iter().map(|v| v[0]).chain(va.iter().map(|v| v[3])).collect();
    let mut vy: Vec<u64> = va.iter().map(|v| v[1]).chain(va.iter().map(|v| v[4])).collect();
    let mut vd: Vec<u64> = va.iter().map(|v| v[2]).chain(va.iter().map(|v| v[5])).collect();

    vx.sort();
    vy.sort();
    vd.sort();
    
    let mut vl: u64 = 0;
    
    for i in 0 .. 2*n-1 {
        for j in 0 .. 2*n-1 {
            for m in 0 .. 2*n-1 {
                let mut ct: usize = 0;
                
                for l in 0 .. n {
                    if va[l][0] <= vx[i] && vx[i+1] <= va[l][3] &&
                        va[l][1] <= vy[j] && vy[j+1] <= va[l][4] &&
                        va[l][2] <= vd[m] && vd[m+1] <= va[l][5] { ct += 1 }
                }

                if ct >= k { vl += (vx[i+1] - vx[i]) * (vy[j+1] - vy[j]) * (vd[m+1] - vd[m]) }
            }
        }
    }
    
    println!("{}", vl);
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
