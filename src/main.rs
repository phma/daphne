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
  let mut zerocrypt:Vec<u8> = Vec::new();
  let mut squarecrypt:Vec<u8> = Vec::new();
  let mut squareplain:Vec<u8> = Vec::new();
  zerodaph.set_key(&[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);
  squaredaph.set_key(&[0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225]);
  let mut squaredaph1=squaredaph.clone();
  let mut squaredaph2=squaredaph.clone();
  for i in 0..=255 {
    stepvec.push(step(i,243,125));
  }
  //printvec(&stepvec);
  println!("Encrypt all 0s with all 0s");
  for i in 0..=255 {
    zerocrypt.push(zerodaph.encrypt(0));
  }
  printvec(&zerocrypt);
  println!("Encrypt 0..255 with squares");
  for i in 0..=255 {
    squarecrypt.push(squaredaph.encrypt(i));
  }
  printvec(&squarecrypt);
  println!("Decrypt the above");
  squareplain=squaredaph1.decrypt_vec(&squarecrypt);
  printvec(&squareplain);
  println!("Demonstrate resynchronization");
  squareplain.clear();
  squarecrypt[0]=2;
  squareplain=squaredaph2.decrypt_vec(&squarecrypt);
  printvec(&squareplain);
}
