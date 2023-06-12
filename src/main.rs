use daphne::*;

fn printvec(k:&[u8]) {
  for i in 0..k.len() {
    print!("{:02x} ",k[i]);
    if i%16==15 || i+1==k.len() {
      println!();
    }
  }
}

fn main() {
  let mut zerodaph=Daphne::new();
  let mut squaredaph=Daphne::new();
  let mut stepvec:Vec<u8> = Vec::new();
  zerodaph.set_key(&[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
  squaredaph.set_key(&[0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225]);
  for i in 0..=255 {
    stepvec.push(step(i,243,125));
  }
    printvec(&stepvec);
}
