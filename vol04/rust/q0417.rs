use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::VecDeque;

fn main(){
    let nm: Vec<usize> = read_vec();
    let n = nm[0];
    let m = nm[1];

    let mut vl: Vec<VecDeque<usize>> = vec![VecDeque::new();n];
    let mut bh: BinaryHeap<Lane> = BinaryHeap::new();

    for i in 0..n {
        bh.push(Lane{lane: i, cars: 0});
    }
    
    for _ in 0..m {
        let cm: Vec<usize> = read_vec();

        if cm[0] == 0 {
            let ln = cm[1] - 1;
            if let Some(i) = vl[ln].pop_front() {
                println!("{}", i);
                bh.push(Lane{lane: ln, cars: vl[ln].len()});
            }
        } else {
            let mut ln = bh.pop().unwrap();
            while ln.cars != vl[ln.lane].len() {
                ln = bh.pop().unwrap();
            }
            vl[ln.lane].push_back(cm[1]);
            bh.push(Lane{lane: ln.lane, cars : ln.cars + 1})
        }
    }
}

#[derive(Debug)]
struct Lane {
    lane: usize,
    cars: usize
}

impl PartialOrd for Lane {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.cars == other.cars {
            other.lane.partial_cmp(&self.lane)
        } else {
            other.cars.partial_cmp(&self.cars)
        }
    }
}

impl Ord for Lane {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(&other).unwrap()
    }
}

impl PartialEq for Lane {
    fn eq(&self, other: &Self) -> bool {
        self.lane == other.lane && self.cars == other.cars
    }
}

impl Eq for Lane {}

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
