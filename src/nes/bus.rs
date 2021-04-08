pub const RAM_END: u16 = RAM_MIRROR_START - 1;
pub const RAM_MIRROR_START: u16 = 0x0800;
pub const RAM_MIRROR_END: u16 = 0x2000 - 1;
pub const PRG_ROM_START: u16 = 0x8000;
pub const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus {
    ram: [u8; 2 * 1024],
    prg_rom: Vec<u8>,
}

pub trait Memory {
    fn write8(&mut self, addr: u16, value: u8);
    fn read8(&self, addr: u16) -> u8;
    fn read16(&self, addr: u16) -> u16;
}

impl Bus {
    pub fn new(prg_rom: Vec<u8>) -> Self {
        return Self {
            ram: [0; 2 * 1024],
            prg_rom,
        };
    }
}

impl Memory for Bus {
    fn write8(&mut self, addr: u16, value: u8) {
        match addr {
            0..=RAM_END => self.ram[addr as usize] = value,
            RAM_MIRROR_START..=RAM_MIRROR_END => {
                self.ram[(addr - RAM_MIRROR_START) as usize] = value
            }
            _ => panic!("Unsupported write @{:X}={:X}", addr, value),
        }
    }

    fn read8(&self, addr: u16) -> u8 {
        match addr {
            0..=RAM_END => self.ram[addr as usize],
            RAM_MIRROR_START..=RAM_MIRROR_END => self.ram[(addr - RAM_MIRROR_START) as usize],
            PRG_ROM_START..=PRG_ROM_END => self.prg_rom[(addr - PRG_ROM_START) as usize],
            _ => panic!("Unsupported read from @{:X}", addr),
        }
    }

    fn read16(&self, addr: u16) -> u16 {
        let lo = self.read8(addr) as u16;
        let hi = self.read8(addr + 1) as u16;

        (hi << 8) | lo
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_read_ram() {
        let mut bus = Bus::new(vec![]);
        bus.write8(0x03, 53);
        assert_eq!(bus.read8(0x03), 53);
        bus.write8(0x03 + RAM_MIRROR_START, 53);
        assert_eq!(bus.read8(0x03 + RAM_MIRROR_START), 53);
    }

    #[test]
    fn test_write_read_ram_mirrors_1() {
        let mut bus = Bus::new(vec![]);
        bus.write8(0x13, 53);
        assert_eq!(bus.read8(0x13 + RAM_MIRROR_START), 53);
    }

    #[test]
    fn test_write_read_ram_mirrors_2() {
        let mut bus = Bus::new(vec![]);
        bus.write8(0x73 + RAM_MIRROR_START, 56);
        assert_eq!(bus.read8(0x73), 56);
    }

    #[test]
    fn test_write_read_ram_long() {
        let mut bus = Bus::new(vec![]);
        bus.write8(0x73, 0x34);
        bus.write8(0x74, 0x12);
        assert_eq!(bus.read16(0x73), 0x1234);
    }
}
