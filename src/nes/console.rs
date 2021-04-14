use crate::nes::bus::Bus;
use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;
use crate::nes::Cartridge;

pub struct Console {
    cpu: Cpu,
    ppu: Ppu,
    ram: [u8; 2048],
    nmi: bool,
    system_cycles: u64,
    cart: Cartridge,
}

impl Console {
    pub fn new(cart: Cartridge) -> Self {
        Self {
            cpu: Cpu::new(),
            ppu: Ppu::new(),
            ram: [0u8; 2048],
            nmi: false,
            system_cycles: 0,
            cart,
        }
    }

    pub fn reset(&mut self, start: Option<u16>) {
        self.system_cycles = 0;
        self.nmi = false;
        self.ppu.reset();

        let mut bus = Bus::new(&mut self.ram, &mut self.nmi, &mut self.cart, &mut self.ppu);
        self.cpu.reset(&mut bus);

        if let Some(addr) = start {
            self.cpu.pc = addr;
        }
    }

    pub fn clock(&mut self) {
        self.ppu.clock(&mut self.nmi);
        if self.system_cycles % 3 == 0 {
            let mut bus = Bus::new(&mut self.ram, &mut self.nmi, &mut self.cart, &mut self.ppu);

            self.cpu.clock(&mut bus);
        }
        self.system_cycles += 1;
    }
}
