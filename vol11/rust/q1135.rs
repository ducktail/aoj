use std::cmp;

fn main(){
  let nds: usize = read();
  
  for _ in 0 .. nds {
    let iof: usize = read();
    let oy: usize = read();
    let nom: usize = read();

    let mut mof: usize = 0;
    
    for _ in 0 .. nom {
      let tv: Vec<f64> = read_vec();
      let tf = tv[0] as usize;
      let air = tv[1];
      let fee = tv[2] as usize;

      let nair: usize = (air * 8192.0) as usize;
      
      let mut of: usize = iof;
      
      if tf == 0 {
        let mut ait: usize = 0;
        
        for _ in 0 .. oy {
          let it: usize = of * nair / 8192;
          ait += it;
          of = of - fee;
        }
        
        mof = cmp::max(mof, of + ait);
      } else {
        for _ in 0 .. oy {
          let it: usize = of * nair / 8192;
          of = of + it - fee;
        }
        
        mof = cmp::max(mof, of);
      }
    }
    println!("{}", mof);
  }
}

fn read<T>() -> T
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.trim().parse().unwrap()
}

fn read_vec<T>() -> Vec<T>
  where T: std::str::FromStr,
        T::Err: std::fmt::Debug
{
  let mut buf = String::new();
  std::io::stdin().read_line(&mut buf).expect("failed to read");
  buf.split_whitespace().map(|e| e.parse().unwrap()).collect()
}
