use super::Mapper;

pub struct MapperZero {
    prg: Vec<u8>,
    chr: Vec<u8>,
}

impl MapperZero {
    pub fn new(prg: Vec<u8>, chr: Vec<u8>) -> Self {
        Self { prg, chr }
    }
}

impl Mapper for MapperZero {
    fn read_cpu(&self, addr: u16) -> Option<u8> {
        if addr >= 0x8000 {
            let masked = addr
                & if self.prg.len() > 0x4000 {
                    0x7FFF
                } else {
                    0x3FFF
                };
            Some(self.prg[masked as usize])
        } else {
            None
        }
    }

    fn read_ppu(&self, addr: u16) -> Option<u8> {
        if addr < 0x2000 {
            Some(self.chr[addr as usize])
        } else {
            None
        }
    }
}
