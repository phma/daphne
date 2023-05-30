use daphne::step;

fn main() {
  for i in 0..=255 {
    print!("{:02x} ",step(i,243,125));
    if i%16==15 {
      println!();
    }
  }
}
