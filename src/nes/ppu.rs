use crate::nes::cartridge::Mirroring;
use crate::nes::ppuregisters::*;
use crate::nes::Cartridge;

pub struct Ppu {
    control: ControlRegister,
    mask: MaskRegister,
    status: StatusRegister,
    cycles: u32,
    scanline: u32,
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
    color_rom: [(u8, u8, u8); 64],
}

impl Ppu {
    pub fn new() -> Self {
        let mut colors = [(0, 0, 0); 64];

        colors[0x00] = (84, 84, 84);
        colors[0x01] = (0, 30, 116);
        colors[0x02] = (8, 16, 144);
        colors[0x03] = (48, 0, 136);
        colors[0x04] = (68, 0, 100);
        colors[0x05] = (92, 0, 48);
        colors[0x06] = (84, 4, 0);
        colors[0x07] = (60, 24, 0);
        colors[0x08] = (32, 42, 0);
        colors[0x09] = (8, 58, 0);
        colors[0x0A] = (0, 64, 0);
        colors[0x0B] = (0, 60, 0);
        colors[0x0C] = (0, 50, 60);
        colors[0x0D] = (0, 0, 0);
        colors[0x0E] = (0, 0, 0);
        colors[0x0F] = (0, 0, 0);
        colors[0x10] = (152, 150, 152);
        colors[0x11] = (8, 76, 196);
        colors[0x12] = (48, 50, 236);
        colors[0x13] = (92, 30, 228);
        colors[0x14] = (136, 20, 176);
        colors[0x15] = (160, 20, 100);
        colors[0x16] = (152, 34, 32);
        colors[0x17] = (120, 60, 0);
        colors[0x18] = (84, 90, 0);
        colors[0x19] = (40, 114, 0);
        colors[0x1A] = (8, 124, 0);
        colors[0x1B] = (0, 118, 40);
        colors[0x1C] = (0, 102, 120);
        colors[0x1D] = (0, 0, 0);
        colors[0x1E] = (0, 0, 0);
        colors[0x1F] = (0, 0, 0);
        colors[0x20] = (236, 238, 236);
        colors[0x21] = (76, 154, 236);
        colors[0x22] = (120, 124, 236);
        colors[0x23] = (176, 98, 236);
        colors[0x24] = (228, 84, 236);
        colors[0x25] = (236, 88, 180);
        colors[0x26] = (236, 106, 100);
        colors[0x27] = (212, 136, 32);
        colors[0x28] = (160, 170, 0);
        colors[0x29] = (116, 196, 0);
        colors[0x2A] = (76, 208, 32);
        colors[0x2B] = (56, 204, 108);
        colors[0x2C] = (56, 180, 204);
        colors[0x2D] = (60, 60, 60);
        colors[0x2E] = (0, 0, 0);
        colors[0x2F] = (0, 0, 0);
        colors[0x30] = (236, 238, 236);
        colors[0x31] = (168, 204, 236);
        colors[0x32] = (188, 188, 236);
        colors[0x33] = (212, 178, 236);
        colors[0x34] = (236, 174, 236);
        colors[0x35] = (236, 174, 212);
        colors[0x36] = (236, 180, 176);
        colors[0x37] = (228, 196, 144);
        colors[0x38] = (204, 210, 120);
        colors[0x39] = (180, 222, 120);
        colors[0x3A] = (168, 226, 144);
        colors[0x3B] = (152, 226, 180);
        colors[0x3C] = (160, 214, 228);
        colors[0x3D] = (160, 162, 160);
        colors[0x3E] = (0, 0, 0);
        colors[0x3F] = (0, 0, 0);

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
            color_rom: colors,
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

    pub fn clock(&mut self, nmi: &mut bool, ram: &mut [u8]) -> bool {
        if self.scanline < 240 {
            if self.cycles < 256 {
                let off = 256 * 3 * self.scanline + self.cycles * 3;
                ram[off as usize] = if self.odd_frame {133} else {234};
                ram[off as usize + 1] = if self.odd_frame {133} else {234};
            }
        } else if self.scanline >= 240 && self.scanline < 261 {
            if self.scanline == 241 && self.cycles == 1 {
                self.status.set_vertical_blank(true);
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

        self.cycles == 241
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
