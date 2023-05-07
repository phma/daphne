use daphne::funSbox;

fn main() {
  for i in 0..=255 {
    print!("{:02x} ",funSbox(i));
    if i%16==15 {
      println!();
    }
  }
}
