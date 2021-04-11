use std::cmp;

fn main(){

    loop {
        let dw: Vec<usize> = read_vec();
        let d = dw[0];
        let w = dw[1];

        if d == 0 && w == 0 { break; }

        let ve: Vec<Vec<usize>> = (0..d).map(|_| read_vec()).collect();
        
        let mut mcp: usize = 0;

        for lw in 3 ..= w {
            for ld in 3 ..= d {
                for bx in 0 ..= w - lw {
                    for by in 0 ..= d - ld {
                        let mut hw: usize = ve[by][bx];
                        let mut hp: usize = ve[by+1][bx+1];

                        for x in bx .. bx + lw {
                            for y in by .. by + ld {
                                if x == bx || x == bx + lw - 1 || y == by || y == by + ld - 1 {
                                    hw = cmp::min(hw, ve[y][x]);
                                } else {
                                    hp = cmp::max(hp, ve[y][x]);
                                }
                            }
                        }

                        if hp < hw {
                            let mut cp = 0;
                            
                            for x in bx + 1 .. bx + lw - 1 {
                                for y in by + 1 .. by + ld -1 {
                                    cp += hw - ve[y][x];
                                }
                            }

                            mcp = cmp::max(mcp, cp);
                        }
                    }
                }
            }
        }
        println!("{}", mcp);
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
