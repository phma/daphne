extern crate lazy_static;
use lazy_static::lazy_static;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

const fn twist(n: u8,k: isize) -> u8 {
  if k>0 {
    n.rotate_left(n.count_ones())
  } else {
    n.rotate_right(n.count_ones())
  }
}

const fn shuffle(n: u8) -> u8 {
  (n&0x54).rotate_left(3) |
  (n&0x28).rotate_left(7) |
  (n&0x02).rotate_left(5) |
  (n&0x80).rotate_left(4) |
  (n&0x01)
}

pub const fn funSbox(n: u8) -> u8 {
  0x6e^shuffle(twist(n^0x25,-1))
}

lazy_static! {
  static ref sbox: [u8; 256] = {
    let mut m: [u8; 256]=[0;256];
    for i in 0..=255 {
      m[i]=funSbox(i as u8);
    }
    m
  };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
