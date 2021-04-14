pub struct Ppu {
    reg: u8,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu { reg: 0 }
    }

    pub fn reset(&mut self) {}
    pub fn clock(&mut self, nmi: &mut bool) {}
    pub fn write_register(&mut self, reg: u8, value: u8) {
        match reg {            
            0x0 /* Controller ($2000) > write */ => self.reg = value,
            0x1 /* Mask ($2001) > write */ => {}       
            0x3 /* OAM address ($2003) > write */ => {}
            0x4 /* OAM data ($2004) <> read/write */ => {}
            0x5 /* Scroll ($2005) >> write x2 */ => {}
            0x6 /* Address ($2006) >> write x2 */ => {}
            0x7 /* Data ($2007) <> read/write */ => {}
 
            _ => { /* ignore */ }
        };
    }
    pub  fn read_register(&self, reg:u8) -> u8 {
        match  reg {
            0x2 /* Status ($2002) < read */ => self.reg,
            0x4 /* OAM data ($2004) <> read/write */ => 0,
            0x7 /* Data ($2007) <> read/write */ => 0,
            _ => 0
        }
    }
    pub fn write_dma_addr(&mut self, source_page: u8) {}
}
