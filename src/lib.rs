pub fn add(left: usize, right: usize) -> usize {
    left + right
}

pub fn twist(byte: u8) -> u8 {
  byte.rotate_left(byte.count_ones())
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
