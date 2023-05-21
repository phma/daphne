use daphne::div_257;

fn main() {
  for i in 0..=255 {
    print!("{:02x} ",div_257(0,i));
    if i%16==15 {
      println!();
    }
  }
}
