use daphne::twist;

fn main() {
  for i in 0..=255 {
    print!("{:02x} ",twist(i));
    if i%16==15 {
      println!();
    }
  }
}
