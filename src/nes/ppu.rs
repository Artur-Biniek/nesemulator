pub struct Ppu {
    reg: u8,
    cycles: u32,
    scanline: i32,
    oam_addr:  u8,
    oam_ram : [u8; 256]
}

impl Ppu {
    pub fn new() -> Self {
        Ppu { reg: 0, scanline :-1, cycles: 0, oam_addr:0, oam_ram: [0u8 ; 256],
         }
    }

    pub fn reset(&mut self) {
        self.scanline = -1;
        self.cycles = 0;
    }

    pub fn clock(&mut self, nmi: &mut bool) {

    }

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
    pub fn dma_write (&mut self, offset   :u8 , data: u8){
    
        self.oam_ram[self.oam_addr.wrapping_add(offset) as usize] = data;
    }
}
