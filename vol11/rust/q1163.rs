const INF: usize = std::usize::MAX;

fn main() {
    loop {
        let mn: Vec<usize> = read_vec();
        let m = mn[0];
        let n = mn[1];

        if m == 0 && n == 0 {
            break;
        }

        let mut vb: Vec<u32> = Vec::new();
        let mut vr: Vec<u32> = Vec::new();

        let mut cnt = 0;

        while cnt < m {
            let t: Vec<u32> = read_vec();
            cnt += t.len();
            for x in t {
                vb.push(x);
            }
        }

        cnt = 0;
        
        while cnt < n {
            let t: Vec<u32> = read_vec();
            cnt += t.len();
            for x in t {
                vr.push(x);
            }
        }
        
        let mut graph: Vec<Vec<Edge>> = vec![Vec::new(); m + n + 2];

        for i in 0..m {
            for j in 0..n {
                if gcd(vb[i], vr[j]) >= 2 {
                    add_edge(&mut graph, i, m + j, 1);
                }
            }

            add_edge(&mut graph, m + n, i, 1);
        }

        for j in 0..n {
            add_edge(&mut graph, m + j, m + n + 1, 1);
        }

        let mf = max_flow(&mut graph, m + n, m + n + 1);
        println!("{}", mf);
    }
}

#[derive(Debug, Copy, Clone)]
struct Edge {
    to: usize,
    cap: usize,
    rev: usize,
}

fn add_edge(g: &mut Vec<Vec<Edge>>, f: usize, t: usize, c: usize) {
    let lt = g[t].len();
    g[f].push(Edge {
        to: t,
        cap: c,
        rev: lt,
    });
    let ft = g[f].len();
    g[t].push(Edge {
        to: f,
        cap: 0,
        rev: ft - 1,
    });
}

fn mf_dfs(g: &mut Vec<Vec<Edge>>, u: &mut Vec<bool>, v: usize, t: usize, f: usize) -> usize {
    use std::cmp::min;

    if v == t {
        f
    } else {
        u[v] = true;

        let lv = g[v].len();
        for i in 0..lv {
            let e: Edge = g[v][i];
            if !u[e.to] && e.cap > 0 {
                let d = mf_dfs(g, u, e.to, t, min(f, e.cap));
                if d > 0 {
                    g[v][i].cap -= d;
                    g[e.to][e.rev].cap += d;
                    return d;
                }
            }
        }

        0
    }
}

fn max_flow(g: &mut Vec<Vec<Edge>>, s: usize, t: usize) -> usize {
    let mut flow: usize = 0;

    loop {
        let mut used: Vec<bool> = vec![false; g.len()];
        let f = mf_dfs(g, &mut used, s, t, INF);
        if f == 0 {
            break flow;
        }
        flow += f;
    }
}

fn gcd(a: u32, b: u32) -> u32 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

#[allow(dead_code)]
fn read_vec<T>() -> Vec<T>
where
    T: std::str::FromStr,
    T::Err: std::fmt::Debug,
{
    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .expect("failed to read");
    buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
