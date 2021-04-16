use crate::nes::cartridge::Mirroring;
use crate::nes::ppuregisters::*;
use crate::nes::Cartridge;

pub struct Ppu {
    control: ControlRegister,
    mask: MaskRegister,
    status: StatusRegister,
    cycles: u32,
    scanline: i32,
    oam_addr: u8,
    oam_ram: [u8; 256],
    vram: [u8; 2 * 1024],
    palette: [u8; 32],
    buffer: u8,
    write_toggle: bool,
    loopy_reg_video_ram: LoopyRegister,
    loopy_reg_temp_ram: LoopyRegister,
    fine_x: u8,
    odd_frame: bool,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            scanline: 0,
            cycles: 0,
            odd_frame: false,

            control: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),

            oam_addr: 0,
            oam_ram: [0; 256],
            vram: [0; 2 * 1024],
            palette: [0; 32],

            buffer: 0,
            write_toggle: false,
            loopy_reg_video_ram: LoopyRegister::new(),
            loopy_reg_temp_ram: LoopyRegister::new(),
            fine_x: 0,
        }
    }

    pub fn reset(&mut self) {
        self.scanline = 0;
        self.cycles = 0;
        self.odd_frame = false;

        self.buffer = 0;

        self.control.register = 0;
        self.mask.register = 0;
        self.status.register = 0;

        self.write_toggle = false;
        self.loopy_reg_video_ram.set_register(0);
        self.loopy_reg_temp_ram.set_register(0);
        self.fine_x = 0;
    }

    pub fn clock(&mut self, nmi: &mut bool) {
        //println!("PPU: {}:{}", self.scanline, self.cycles);
        if self.scanline < 240 {
        } else if self.scanline >= 240 && self.scanline < 261 {
            if self.scanline == 241 && self.cycles == 1 {
                self.status.set_vertical_blank(true);

                println!("VBLANK - start");
                if self.control.nmi_enabled() {
                    *nmi = true;
                }
            }
        } else {
            // 261
            if self.cycles == 1 {
                println!("VBLANK - finish");
                self.status.set_vertical_blank(false);
                self.status.set_sprite_zero_hit(false);
            }
        }

        self.cycles += 1;
        if self.cycles >= 341 {
            self.cycles = 0;
            self.scanline += 1;

            if self.scanline >= 262 {
                self.scanline = 0;
                self.odd_frame = !self.odd_frame;

                println!("FRAME FINISHED");
            }
        }
    }

    pub fn write_register(&mut self, cart: &mut Cartridge, reg: u8, value: u8) {
        match reg {
            0x0 /* Controller ($2000) > write */ => {
                self.control.register = value;
                self.loopy_reg_temp_ram.set_nametable_x(self.control.nametable_x());
                self.loopy_reg_temp_ram.set_nametable_y(self.control.nametable_y());
            }
            0x1 /* Mask ($2001) > write */ => {
                self.mask.register = value;
            }
            0x3 /* OAM address ($2003) > write */ => {
                self.oam_addr = value;
            }
            0x4 /* OAM data ($2004) <> read/write */ => {
                self.oam_ram[self.oam_addr as usize] = value;
            }
            0x5 /* Scroll ($2005) >> write x2 */ => {
                if !self.write_toggle {
                    self.fine_x = value & 0x7;
                    self.loopy_reg_temp_ram.set_coarse_x(value >> 3);
                    self.write_toggle = true;
                } else {
                    self.loopy_reg_temp_ram.set_fine_y(value & 0x7);
                    self.loopy_reg_temp_ram.set_coarse_y(value >> 3);
                    self.write_toggle = false;
                }
            }
            0x6 /* Address ($2006) >> write x2 */ => {
                if !self.write_toggle {
                    self.loopy_reg_temp_ram.set_register(((value as u16 & 0x3F) << 8) | ( self.loopy_reg_temp_ram.register() & 0x00FF));
                    self.write_toggle = true;
                } else {
                    self.loopy_reg_temp_ram.set_register((self.loopy_reg_temp_ram.register() & 0xFF00) | value as u16);
                    self.loopy_reg_video_ram.set_register(self.loopy_reg_temp_ram.register());
                    self.write_toggle = false;
                }
            }
            0x7 /* Data ($2007) <> read/write */ => {
                let lvram =  self.loopy_reg_video_ram.register();
                self.write(cart, lvram, value);
                self.loopy_reg_video_ram.set_register(lvram +  if self.control.increment_mode()  {32} else {1});
            }
            _ => { /* ignore */ }
        };
    }

    pub fn read_register(&mut self, cart: &Cartridge, reg: u8, debug: bool) -> u8 {
        if debug {
            match  reg {
                0x2 /* Status ($2002) < read */ => {
                    (self.status.register & 0xE0) | (self.buffer & 0x1F)
                }
                0x4 /* OAM data ($2004) <> read/write */ => {
                    self.oam_ram[self.oam_addr as usize]
                }
                0x7 /* Data ($2007) <> read/write */ => {
                    let mut data = self.buffer;

                    let lvram = self.loopy_reg_video_ram.register() ;
                    let buffer = self.read(cart, lvram);

                    if lvram >= 0x3F00 {data = buffer;}

                    data
                },
                _ => 0
            }
        } else {
            match  reg {
                0x2 /* Status ($2002) < read */ => {
                    let data =  (self.status.register & 0xE0) | (self.buffer & 0x1F);
                    self.status.set_vertical_blank(false);
                    self.write_toggle = false;
                    data
                }
                0x4 /* OAM data ($2004) <> read/write */ => {
                    self.oam_ram[self.oam_addr as usize]
                }
                0x7 /* Data ($2007) <> read/write */ => {
                    let mut data = self.buffer;

                    let lvram = self.loopy_reg_video_ram.register() ;
                    self.buffer = self.read(cart, lvram);

                    if lvram >= 0x3F00 {data = self.buffer;}

                    self.loopy_reg_video_ram.set_register(lvram +  if self.control.increment_mode()  {32} else {1});

                    data
                },
                _ => 0
            }
        }
    }

    pub fn dma_write(&mut self, offset: u8, data: u8) {
        self.oam_ram[self.oam_addr.wrapping_add(offset) as usize] = data;
    }

    fn write(&mut self, cart: &mut Cartridge, addr: u16, value: u8) {
        if let Some(v) = cart.write_ppu(addr, value) {
            v
        } else {
            match addr {
                0x2000..=0x3EFF => {
                    let masked = addr & 0x0FFF;

                    if cart.mirroring == Mirroring::Vertical {
                        match masked {
                            0x0000..=0x07FF => self.vram[masked as usize] = value,
                            0x0800..=0x0FFF => self.vram[masked as usize & 0x07FF] = value,
                            _ => panic!("Oh, no!"),
                        }
                    } else if cart.mirroring == Mirroring::Horizontal {
                        match masked {
                            0x0000..=0x03FF => self.vram[masked as usize] = value,
                            0x0400..=0x07FF => self.vram[masked as usize & 0x03FF] = value,
                            0x0800..=0x0BFF => self.vram[masked as usize & 0x03FF | 0x0400] = value,
                            0x0C00..=0x0FFF => self.vram[masked as usize & 0x03FF | 0x0400] = value,
                            _ => panic!("Oh, no!"),
                        }
                    }

                    panic!("Oh, no!");
                }
                0x3F00..=0x3FFF => {
                    let mut masked = addr & 0x1F;

                    if masked == 0x0010 {
                        masked = 0x0000;
                    }
                    if masked == 0x0014 {
                        masked = 0x0004;
                    }
                    if masked == 0x0018 {
                        masked = 0x0008;
                    }
                    if masked == 0x001C {
                        masked = 0x000C;
                    }

                    self.palette[masked as usize] = value;
                }
                _ => { /* no op */ }
            }
        }
    }

    fn read(&self, cart: &Cartridge, addr: u16) -> u8 {
        /*
        $2000-$23FF	$0400	Nametable 0
        $2400-$27FF	$0400	Nametable 1
        $2800-$2BFF	$0400	Nametable 2
        $2C00-$2FFF	$0400	Nametable 3
        $3000-$3EFF	$0F00	Mirrors of $2000-$2EFF
        $3F00-$3F1F	$0020	Palette RAM indexes
        $3F20-$3FFF	$00E0	Mirrors of $3F00-$3F1F
        */
        if let Some(v) = cart.read_ppu(addr) {
            v
        } else {
            match addr {
                0x2000..=0x3EFF => {
                    let masked = addr & 0x0FFF;

                    if cart.mirroring == Mirroring::Vertical {
                        match masked {
                            0x0000..=0x07FF => self.vram[masked as usize],
                            0x0800..=0x0FFF => self.vram[masked as usize & 0x07FF],
                            _ => panic!("Oh, no!"),
                        }
                    } else if cart.mirroring == Mirroring::Horizontal {
                        match masked {
                            0x0000..=0x03FF => self.vram[masked as usize],
                            0x0400..=0x07FF => self.vram[masked as usize & 0x03FF],
                            0x0800..=0x0BFF => self.vram[masked as usize & 0x03FF | 0x0400],
                            0x0C00..=0x0FFF => self.vram[masked as usize & 0x03FF | 0x0400],
                            _ => panic!("Oh, no!"),
                        }
                    } else {
                        panic!("Oh, no!")
                    }
                }
                0x3F00..=0x3FFF => {
                    let mut masked = addr & 0x1F;

                    if masked == 0x0010 {
                        masked = 0x0000;
                    }
                    if masked == 0x0014 {
                        masked = 0x0004;
                    }
                    if masked == 0x0018 {
                        masked = 0x0008;
                    }
                    if masked == 0x001C {
                        masked = 0x000C;
                    }

                    self.palette[masked as usize]
                        & (if self.mask.gray_scale() { 0x30 } else { 0x3F })
                }
                _ => 0u8,
            }
        }
    }
}
