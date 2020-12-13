use std::cmp::Ordering;
use std::collections::BinaryHeap;

fn main(){
    loop {
        let n: usize = read();
        if n == 0 { break }
        
        let vc: Vec<Vec<f64>> = (0..n).map(|_| read_vec()).collect();

        let mut stl: f64 = 0.0;
        let mut ven: Vec<bool> = vec![true; n];
        let mut bh: BinaryHeap<Cell> = BinaryHeap::new();

        bh.push(Cell{dist: 0.0, node: 0});
        
        loop {
            if let Some(c) = bh.pop() {
                if ven[c.node] {
                    ven[c.node] = false;
                    stl += c.dist;

                    for i in 0 .. n {
                        if ven[i] {
                            bh.push(Cell{ dist: dist(&vc, c.node, i), node: i});
                        }
                    }
                }
            } else {
                break
            }
        }
        
        println!("{:.3}", stl);
    }
}

#[derive(PartialEq, Debug)]
struct Cell {
    dist: f64,
    node: usize
}

impl Eq for Cell {}

impl PartialOrd for Cell {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.dist.partial_cmp(&self.dist)
    }
}

impl Ord for Cell {
    fn cmp(&self, other: &Cell) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn dist(v: &Vec<Vec<f64>>, i: usize, j: usize) -> f64 {
    let d: f64 = ((v[i][0] - v[j][0]) * (v[i][0] - v[j][0]) +
                  (v[i][1] - v[j][1]) * (v[i][1] - v[j][1]) +
                  (v[i][2] - v[j][2]) * (v[i][2] - v[j][2])).sqrt() - v[i][3] - v[j][3];
    if d < 0.0 { 0.0 } else { d }
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
