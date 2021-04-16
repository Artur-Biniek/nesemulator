pub struct BitReg<T> {
    pub reg: T,
}

impl<T> BitReg<T> {
    pub fn new(reg: T) -> Self {
        Self { reg }
    }
}

pub trait BitRange<T> {
    fn bits(&self, msb: usize, lsb: usize) -> T;
    fn set_bits(&mut self, msb: usize, lsb: usize, value: T);
}

impl BitRange<u8> for BitReg<u8> {
    fn bits(&self, msb: usize, lsb: usize) -> u8 {
        let width = msb - lsb + 1;
        let mask = ((1u16 << width) - 1) as u8;
        (self.reg >> lsb) & mask
    }

    fn set_bits(&mut self, msb: usize, lsb: usize, value: u8) {
        let width = msb - lsb + 1;
        let mut mask = ((1u16 << width) - 1) as u8;
        mask <<= lsb;
        let cleared = !mask & self.reg;
        let shifted = mask & (value << lsb);
        self.reg = cleared | shifted;
    }
}

impl BitRange<u16> for BitReg<u16> {
    fn bits(&self, msb: usize, lsb: usize) -> u16 {
        let width = msb - lsb + 1;
        let mask = ((1u32 << width) - 1) as u16;
        (self.reg >> lsb) & mask
    }

    fn set_bits(&mut self, msb: usize, lsb: usize, value: u16) {
        let width = msb - lsb + 1;
        let mask = ((1u32 << width) - 1) as u16;
        let cleared = !(mask << lsb) & self.reg;
        let shifted = value << lsb;
        self.reg = cleared | shifted;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let r = BitReg::new(0u8);
        assert_eq!(r.bits(7, 0), 0);
    }

    #[test]
    fn test2() {
        let r = BitReg::new(0b0001_1001u8);
        assert_eq!(r.bits(4, 3), 3);
        assert_eq!(r.bits(4, 2), 6);
        assert_eq!(r.bits(4, 1), 12);
        assert_eq!(r.bits(4, 0), 25);
        assert_eq!(r.bits(7, 0), r.reg);
        assert_eq!(r.bits(7, 5), 0);
        assert_eq!(r.bits(7, 4), 1);
        assert_eq!(r.bits(7, 3), 3);
    }

    #[test]
    fn test3() {
        let mut r = BitReg::new(0b0100_0010u8);
        r.set_bits(7, 7, 1);
        assert_eq!(r.bits(7, 0), 0b1100_0010);
        r.set_bits(7, 7, 0);
        assert_eq!(r.bits(7, 0), 0b0100_0010);
        r.set_bits(1, 1, 0);
        assert_eq!(r.bits(7, 0), 0b0100_0000);
        r.set_bits(3, 1, 0b101);
        assert_eq!(r.bits(7, 0), 0b0100_1010);
        r.set_bits(7, 4, 0b1011);
        assert_eq!(r.bits(7, 0), 0b1011_1010);
    }

    #[test]
    fn test4() {
        let mut r = BitReg::new(0b0100_0010u16);
        r.set_bits(7, 7, 1);
        assert_eq!(r.bits(7, 0), 0b1100_0010);
        r.set_bits(7, 7, 0);
        assert_eq!(r.bits(7, 0), 0b0100_0010);
        r.set_bits(1, 1, 0);
        assert_eq!(r.bits(7, 0), 0b0100_0000);
        r.set_bits(3, 1, 0b101);
        assert_eq!(r.bits(7, 0), 0b0100_1010);
        r.set_bits(7, 4, 0b1011);
        assert_eq!(r.bits(7, 0), 0b1011_1010);
    }

    #[test]
    fn test5() {
        let mut r = BitReg::new(0u16);
        r.set_bits(15, 8, 0b10100101);
        assert_eq!(r.reg, 0b10100101_00000000u16);
    }
}
