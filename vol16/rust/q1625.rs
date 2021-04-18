use std::cmp;

fn main(){
    loop {
        let nmtp: Vec<usize> = read_vec();

        if nmtp.iter().sum::<usize>() == 0 { break; }

        let mut w = nmtp[0];
        let mut h = nmtp[1];
        let t = nmtp[2];
        let p = nmtp[3];

        let mut v: Vec<Vec<usize>> = vec![vec![0 ; 700] ; 700];
        let mut ox: usize = 0;
        let mut oy: usize = 0;

        for x in 0 .. w {
            for y in 0 .. h {
                v[y][x] = 1;
            }
        }
        
        for _ in 0 .. t {
            let dc : Vec<usize> = read_vec();
            let c = dc[1];
            
            if dc[0] == 1 {
                let nox = ox + c;
                
                for y in oy .. oy + h {
                    for x in ox .. nox {
                        v[y][2 * nox - 1 - x] += v[y][x]
                    }
                }

                ox = nox;
                w = cmp::max(w - c, c);
            } else {
                let noy = oy + c;

                for x in ox .. ox + w {
                    for y in oy .. noy {
                        v[2 * noy - 1 - y][x] += v[y][x]
                    }
                }

                oy = noy;
                h = cmp::max(h - c, c);
            }
        }

        for _ in 0 .. p {
            let xy : Vec<usize> = read_vec();

            println!("{}", v[oy + xy[1]][ox + xy[0]]);
        }
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
