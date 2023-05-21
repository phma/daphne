use daphne::div_odd;

fn main() {
  for i in 0..=255 {
    print!("{:02x} ",div_odd(0,i));
    if i%16==15 {
      println!();
    }
  }
}
