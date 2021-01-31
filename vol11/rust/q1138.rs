use std::collections::BinaryHeap;
use std::cmp::Ordering;

fn main(){
    loop {
        let d: Vec<usize> = read_vec();

        if d.iter().sum::<usize>() == 0 { break; }
        
        let n = d[0];
        let m = d[1];
        let p = d[2];
        let a = d[3] - 1;
        let b = d[4] - 1;

        let his: Vec<f64> = read_vec();
            
        let mut td: Vec<Vec<f64>> = vec![vec![10000.0; m]; 1 << n];

        let mut ad: Vec<Vec<(usize, f64)>> = vec![Vec::new(); m];

        for _ in 0 .. p {
            let d: Vec<usize> = read_vec();
            let x = d[0] - 1;
            let y = d[1] - 1;
            let z = d[2] as f64;
            
            ad[x].push((y, z));
            ad[y].push((x, z));
        }

        let mut bh: BinaryHeap<Time> = BinaryHeap::new();
        bh.push(Time{ time: 0.0, tickets: (1 << n) - 1, city: a});

        while let Some(st) = bh.pop() {
            if st.time < td[st.tickets][st.city] {
                td[st.tickets][st.city] = st.time;
                
                for &(i, t1) in ad[st.city].iter() {
                    for j in 0 .. n {
                        if (1 << j) & st.tickets > 0 {
                            let tk = !(1 << j) & st.tickets;
                            let t2 = st.time + t1 / his[j];
                            if t2 < td[tk][i] {
                                bh.push(Time{ time: t2, tickets: tk, city: i});
                            }
                        }
                    }
                }
            }
        }

        let mut ans: f64 = td[0][b];
        for i in 1 .. (1 << n) {
            if ans > td[i][b] {
                ans = td[i][b];
            }
        }

        if ans > 9000.0 {
            println!("Impossible");
        } else {
            println!("{}", ans);
        }
    }
}

#[derive(Debug)]
struct Time {
    time: f64,
    tickets: usize,
    city: usize,
}

impl PartialOrd for Time {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.time.partial_cmp(&self.time)
    }
}

impl Ord for Time {
    fn cmp(&self, other: &Self) -> Ordering {
        other.partial_cmp(&self).unwrap()
    }
}

impl PartialEq for Time {
    fn eq(&self, other: &Self) -> bool {
        (self.time - other.time).abs() <= 0.0000000001
    }
}

impl Eq for Time {}

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
