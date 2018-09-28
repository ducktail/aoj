use std::io;
use std::collections::HashMap;

fn main(){
  loop {
    let v = read_vu32();
    let mut x = v[0];
    let y = v[1];
    
    if x == 0 && y == 0 {break;}

    let mut hm: HashMap<u32, u32> = HashMap::new();
    let mut i:u32 = 0;

    loop {
      if x == 0 {
        println!("{} {}", i, 0);
        break;
      }

      match hm.get(&x) {
        Some(&j) =>  {
          println!("{} {}", j, i - j);
          break;
        },
        None => {
          hm.insert(x, i);
          x = x * 10 % y;
          i += 1;
        }
      }
    }
  }
}

fn read_vu32() -> Vec<u32> {
   let mut buf = String::new();
   io::stdin().read_line(&mut buf).expect("read error");
   buf.trim().split_whitespace().map(|e| e.parse().unwrap()).collect()
}
