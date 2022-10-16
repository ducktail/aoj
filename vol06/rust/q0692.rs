use std::collections::VecDeque;

fn main(){
    let nq: Vec<usize> = read_vec();
    let n = nq[0];
    let q = nq[1];

    let mut cnt: Vec<i32> = vec![-1; 3usize.pow(n as u32)];
    let mut que: VecDeque<usize> = VecDeque::new();

    for i in 0 ..= n {
        for j in 0 ..= n - i {
            let x = (0..n).fold(0, |a, k|{if k < i {a * 3} else if k < i + j {a * 3 + 1} else {a * 3 + 2}});
            cnt[x] = 0;
            que.push_back(x);
        }
    }

    while ! que.is_empty() {
        let x = que.pop_front().unwrap();
        let v0 = int_to_vector(x, n);

        for i in 1 .. n {
            let mut j = 0;
            let mut k = i;
            let mut v1 = v0.clone();
            
            while j < k {
                v1.swap(j, k);
                j += 1;
                k -= 1;
            }
            
            let y = vector_to_int(&v1);
            
            if cnt[y] < 0 {
                cnt[y] = cnt[x] + 1;
                que.push_back(y);
            }
        }
    }

    for _ in 0 .. q {
        let s: String = read();
        println!("{}", cnt[string_to_int(&s)]);
    }
}

fn string_to_int(s: &str) -> usize {
    s.chars().fold(0, |a, c| {a * 3 + match c {'A' => 0, 'B' => 1, _ => 2}})
}

fn vector_to_int(v: &Vec<usize>) -> usize {
    v.iter().fold(0, |a, &x| {a * 3 + x})
}

fn int_to_vector(mut x: usize, n: usize) -> Vec<usize> {
    let mut v = vec![0; n];
    for i in (0 .. n).rev() {
        v[i] = x % 3;
        x /= 3;
    }
    v
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
