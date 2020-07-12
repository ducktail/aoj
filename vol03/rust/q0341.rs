fn main(){
    let m: u64 = 1000000007;
    
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).expect("failed to read");
    let vt: Vec<char> = buf.trim().chars().collect();

    buf.clear();
    std::io::stdin().read_line(&mut buf).expect("failed to read");
    let vb: Vec<char> = buf.trim().chars().collect();
    let vbl = vb.len();
    
    let mut vcnt: Vec<u64> = vec![0; vbl];

    for i in 0 .. vt.len() {
        for j in (0 .. vbl).rev() {
            if vt[i] == vb[j] {
                if j == 0 {
                    vcnt[j] = (vcnt[j] + 1) % m;
                } else {
                    vcnt[j] = (vcnt[j] + vcnt[j-1]) % m;
                }
            }
        }
    }

    println!("{}", vcnt[vbl-1]);
}
