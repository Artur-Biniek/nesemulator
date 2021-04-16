use crate::nes::bus::{Bus, Dma, DmaBus, MemoryRead};
use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;
use crate::nes::Cartridge;

const VIDEO_SIZE: usize = 3 * 256 * 240;

pub struct Console {
    cpu: Cpu,
    ppu: Ppu,
    ram: [u8; 2048],
    nmi: bool,
    dma: Dma,
    system_cycles: u64,
    cart: Cartridge,
    pub video: [u8; VIDEO_SIZE],
}

impl Console {
    pub fn new(cart: Cartridge) -> Self {
        Self {
            cpu: Cpu::new(),
            ppu: Ppu::new(),
            ram: [0u8; 2048],
            nmi: false,
            dma: Dma::Off,
            system_cycles: 0,
            cart,
            video: [0; VIDEO_SIZE],
        }
    }

    pub fn reset(&mut self, start: Option<u16>) {
        self.system_cycles = 0;
        self.nmi = false;
        self.dma = Dma::Off;

        let mut bus = Bus::new(
            &mut self.ram,
            &mut self.nmi,
            &mut self.dma,
            &mut self.cart,
            &mut self.ppu,
        );
        self.cpu.reset(&mut bus);
        self.ppu.reset();

        if let Some(addr) = start {
            self.cpu.pc = addr;
        }
    }

    pub fn clock(&mut self) -> bool {
        let frame_ready = self.ppu.clock(&mut self.nmi, &mut self.video);
        if self.system_cycles % 3 == 0 {
            if let Dma::Off = self.dma {
                let mut bus = Bus::new(
                    &mut self.ram,
                    &mut self.nmi,
                    &mut self.dma,
                    &mut self.cart,
                    &mut self.ppu,
                );
                self.cpu.clock(&mut bus);
            } else {
                match self.dma {
                    Dma::Requested(page) if (self.system_cycles & 1) == 0 => {
                        self.dma = Dma::Read(page, 0);
                    }
                    Dma::Read(page, offset) => {
                        let mut bus = DmaBus::new(&mut self.ram, &mut self.cart, &mut self.ppu);
                        let data = bus.read((page as u16) << 8 | offset as u16);
                        self.dma = Dma::Write(page, offset, data);
                    }
                    Dma::Write(page, offset, data) => {
                        self.ppu.dma_write(offset, data);
                        self.dma = if offset < u8::MAX {
                            Dma::Read(page, offset.wrapping_add(1))
                        } else {
                            Dma::Off
                        }
                    }
                    _ => panic!("Invalid DMA state"),
                }
            }
        }
        self.system_cycles += 1;
        frame_ready
    }
}
