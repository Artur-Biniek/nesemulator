use super::cartridge::Cartridge;
use super::ppu::Ppu;

pub const RAM_END: u16 = 0x2000 - 1;
pub const PPU_REGISTERS: u16 = 0x2000;
pub const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
pub const PRG_ROM_START: u16 = 0x8000;
pub const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus<'a> {
    ram: &'a mut [u8; 2 * 1024],
    nmi: &'a mut bool,
    cartridge: &'a mut Cartridge,
    ppu: &'a mut Ppu,
}

pub trait Memory {
    fn write(&mut self, addr: u16, value: u8);
    fn read(&self, addr: u16) -> u8;
}

impl<'a> Bus<'a> {
    pub fn new(
        ram: &'a mut [u8; 2048],
        nmi: &'a mut bool,
        cartridge: &'a mut Cartridge,
        ppu: &'a mut Ppu,
    ) -> Self {
        return Self {
            ram,
            nmi,
            cartridge,
            ppu,
        };
    }
}

impl Memory for Bus<'_> {
    fn write(&mut self, addr: u16, value: u8) {
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

    fn read(&self, addr: u16) -> u8 {
        if let Some(val) = self.cartridge.read_cpu(addr) {
            val
        } else {
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

                _ => panic!("Unsupported read from @{:X}", addr),
            }
        }
    }
}
