use std::io;
use std::collections::HashMap;

fn main(){
   loop {
        let v = read_vu();
        let n = v[0];
        let m = v[1];

        if n == 0 && m == 0 {break;}

        let vh = make_acv(n);
        let vw = make_acv(m);

        let mh = make_hm(n, vh);
        let mw = make_hm(m, vw);

        let mut cnt = 0;
        
        for (hl, ch) in &mh {
            cnt += match mw.get(&hl) {
                Some(cw) => ch * cw,
                None => 0,
            };
        }
        println!("{}", cnt);
   }
}

fn read_vu() -> Vec<usize> {
   let mut buf = String::new();
   io::stdin().read_line(&mut buf).expect("read error");
   buf.trim().split_whitespace().map(|e| e.parse().unwrap()).collect()
}

fn make_acv(l:usize) -> Vec<u32> {
   let mut v: Vec<u32> = vec![0; l + 1];
   for i in 0 .. l {
       let mut buf = String::new();
       io::stdin().read_line(&mut buf).expect("read error");
       let x: u32 = buf.trim().parse().unwrap();
       v[i+1] = v[i] + x;
   }
   v
}

fn make_hm(l: usize, v:Vec<u32>) -> HashMap<u32, u32> {
   let mut map: HashMap<u32, u32> = HashMap::new();
   for i in 0..l {
       for j in i+1 .. l+1 {
           let d = v[j] - v[i];
           let cnt = map.entry(d).or_insert(0);
           *cnt += 1;
       }
   }
   map
}
