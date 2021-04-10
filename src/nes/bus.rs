use super::cartridge::Rom;

pub const RAM_END: u16 = 0x2000 - 1;
pub const PPU_REGISTERS: u16 = 0x2000;
pub const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
pub const PRG_ROM_START: u16 = 0x8000;
pub const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus {
    ram: [u8; 2 * 1024],
    prg_rom: Rom,
}

pub trait Memory {
    fn write8(&mut self, addr: u16, value: u8);
    fn read8(&self, addr: u16) -> u8;
}

impl Bus {
    pub fn new(prg_rom: Rom) -> Self {
        return Self {
            ram: [0; 2 * 1024],
            prg_rom,
        };
    }

    fn read_prg_rom(&self, address: u16) -> u8 {
        let mirror = address & (0x4000 - 1);
        return self.prg_rom.prg_rom[mirror as usize];
    }
}

impl Memory for Bus {
    fn write8(&mut self, addr: u16, value: u8) {
        match addr {
            0..=RAM_END => {
                let mirror_insensitive = addr & 0x07FF;
                self.ram[mirror_insensitive as usize] = value
            }

            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                // todo!("PPU")
                ()
            }

            _ => panic!("Unsupported write @{:X}={:X}", addr, value),
        }
    }

    fn read8(&self, addr: u16) -> u8 {
        match addr {
            0..=RAM_END => {
                let mirror_insensitive = addr & 0x07FF;
                self.ram[mirror_insensitive as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                //todo!("PPU" );
                0
            }
            PRG_ROM_START..=PRG_ROM_END => self.read_prg_rom(addr - 0x8000),
            _ => panic!("Unsupported read from @{:X}", addr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::cartridge::test as rt;

    #[test]
    fn test_write_read_ram() {
        let mut bus = Bus::new(rt::test_rom());
        bus.write8(0x03, 53);
        assert_eq!(bus.read8(0x03), 53);
        bus.write8(0x03 + 0x0800, 53);
        assert_eq!(bus.read8(0x03 + 0x0800), 53);
    }

    #[test]
    fn test_write_read_ram_mirrors_1() {
        let mut bus = Bus::new(rt::test_rom());
        bus.write8(0x13, 53);
        assert_eq!(bus.read8(0x13 + 0x0800), 53);
    }

    #[test]
    fn test_write_read_ram_mirrors_2() {
        let mut bus = Bus::new(rt::test_rom());
        bus.write8(0x73 + 0x1800, 56);
        assert_eq!(bus.read8(0x73), 56);
    }
}
