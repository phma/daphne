use daphne::*;

fn main() {
  let mut zerodaph=Daphne::new();
  let mut squaredaph=Daphne::new();
  squaredaph.set_key(&[0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225]);
  for i in 0..=255 {
    print!("{:02x} ",step(i,243,125));
    if i%16==15 {
      println!();
    }
  }
}
