extern crate lazy_static;
use lazy_static::lazy_static;

// Rotating a word of at least 3 bits by its population count
// satisfies the strict avalanche criterion.
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

pub fn mul_odd(a: u8,b: u8) -> u8 {
  let aw=a as u32;
  let bw=b as u32;
  ((aw+bw+2*aw*bw)&0xff) as u8
}

pub fn mul_257(a: u8,b: u8) -> u8 {
  let aw=if a==0 {256} else {a as u32};
  let bw=if b==0 {256} else {b as u32};
  ((aw*bw%257)&0xff) as u8
}

lazy_static! {
  static ref SBOX: [u8; 256] = {
    let mut m: [u8; 256]=[0;256];
    for i in 0..=255 {
      m[i]=funSbox(i as u8);
    }
    m
  };
  static ref INV_SBOX: [u8; 256] = {
    let mut m: [u8; 256]=[0;256];
    for i in 0..=255 {
      m[funSbox(i) as usize]=i;
    }
    m
  };
  static ref INV_ODD: [u8; 256] = {
    let mut m: [u8; 256]=[0;256];
    for i in 0..=255 {
      for j in 0..=255 {
	if mul_odd(i,j)==0 {
	  m[i as usize]=j;
	}
      }
    }
    m
  };
  static ref INV_257: [u8; 256] = {
    let mut m: [u8; 256]=[0;256];
    for i in 0..=255 {
      for j in 0..=255 {
	if mul_257(i,j)==1 {
	  m[i as usize]=j;
	}
      }
    }
    m
  };
}

pub fn div_odd(a: u8,b: u8) -> u8 {
  mul_odd(a,INV_ODD[b as usize])
}

pub fn div_257(a: u8,b: u8) -> u8 {
  mul_odd(a,INV_257[b as usize])
}

pub fn step(x: u8,l: u8,r: u8) -> u8 {
  mul_odd(SBOX[mul_257(x,l) as usize],r)
}

pub fn inv_step(x: u8,l: u8,r: u8) -> u8 {
  div_257(INV_SBOX[div_odd(x,r) as usize],l)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sbox() {
        let result = SBOX[0xf5];
        assert_eq!(result, 0xaa);
        let result = INV_SBOX[0xaa];
        assert_eq!(result, 0xf5);
    }
}
