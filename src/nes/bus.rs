use super::cartridge::Cartridge;
use super::ppu::Ppu;

pub const RAM_END: u16 = 0x2000 - 1;
pub const PPU_REGISTERS: u16 = 0x2000;
pub const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
pub const PPU_DMA_ADDR: u16 = 0x4014;
pub const PRG_ROM_START: u16 = 0x8000;
pub const PRG_ROM_END: u16 = 0xFFFF;

pub struct Bus<'a> {
    ram: &'a mut [u8; 2 * 1024],
    nmi: &'a mut bool,
    dma: &'a mut Dma,
    cartridge: &'a mut Cartridge,
    ppu: &'a mut Ppu,
    trace: bool,
}

impl<'a> NmiHandler for Bus<'_> {
    fn read_clear_nmi(&mut self) -> bool {
        let res = *self.nmi;
        *self.nmi = false;
        res
    }
}

pub struct DmaBus<'a> {
    ram: &'a mut [u8; 2 * 1024],
    cartridge: &'a mut Cartridge,
    ppu: &'a mut Ppu,
    trace: bool,
}

pub trait MemoryRead {
    fn read(&mut self, addr: u16) -> u8;
    fn set_trace(&mut self, trace: bool);
}

pub trait MemoryWrite {
    fn write(&mut self, addr: u16, value: u8);
}

pub trait NmiHandler {
    fn read_clear_nmi(&mut self) -> bool;
}

pub type DmaOffset = u8;
pub type DmaPage = u8;

pub enum Dma {
    Off,
    Requested(DmaPage),
    Write(DmaPage, DmaOffset, u8),
    Read(DmaPage, u8),
}

impl<'a> Bus<'a> {
    pub fn new(
        ram: &'a mut [u8; 2048],
        nmi: &'a mut bool,
        dma: &'a mut Dma,
        cartridge: &'a mut Cartridge,
        ppu: &'a mut Ppu,
    ) -> Self {
        return Self {
            ram,
            nmi,
            dma,
            cartridge,
            ppu,
            trace: false,
        };
    }
}

impl<'a> DmaBus<'a> {
    pub fn new(ram: &'a mut [u8; 2048], cartridge: &'a mut Cartridge, ppu: &'a mut Ppu) -> Self {
        return Self {
            ram,
            cartridge,
            ppu,
            trace: false,
        };
    }
}

impl MemoryRead for DmaBus<'_> {
    fn read(&mut self, addr: u16) -> u8 {
        read_memory(self.ram, self.cartridge, self.ppu, addr, self.trace)
    }
    fn set_trace(&mut self, trace: bool) {
        self.trace = trace;
    }
}

impl MemoryWrite for Bus<'_> {
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

            0x4000..=0x4013 | 0x4015 | 0x4017 => {
                //APU
            }

            PPU_DMA_ADDR => *self.dma = Dma::Requested(value),

            _ => panic!("Unsupported write @{:X}={:X}", addr, value),
        }
    }
}

impl MemoryRead for Bus<'_> {
    fn read(&mut self, addr: u16) -> u8 {
        read_memory(&self.ram, &self.cartridge, &mut self.ppu, addr, self.trace)
    }
    fn set_trace(&mut self, trace: bool) {
        self.trace = trace;
    }
}

fn read_memory(
    ram: &[u8; 2048],
    cartridge: &Cartridge,
    ppu: &mut Ppu,
    addr: u16,
    trace: bool,
) -> u8 {
    if let Some(val) = cartridge.read_cpu(addr) {
        val
    } else {
        match addr {
            0..=RAM_END => {
                let mirror_insensitive = addr & 0x07FF;
                ram[mirror_insensitive as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let reg = addr & 0x7;
                ppu.read_register(cartridge, reg as u8, trace)
            }
            0x4015 => {
                // APU
                0
            }
            0x4016..=0x4017 => {
                // Controllers
                0
            }

            _ => panic!("Unsupported read from @{:X}", addr),
        }
    }
}
