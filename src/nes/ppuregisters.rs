use super::bits::{BitRange, BitReg};

const CONTROL_FLAG_NAMETABLE_X: u8 = 0b0000_0001;
const CONTROL_FLAG_NAMETABLE_Y: u8 = 0b0000_0010;
const CONTROL_FLAG_INC_MODE: u8 = 0b0000_0100;
// const CONTROL_FLAG_PTRN_SPRITE: u8 = 0b0000_1000;
// const CONTROL_FLAG_PTRN_BGRND: u8 = 0b0001_0000;
// const CONTROL_FLAG_SPRITE_SIZE: u8 = 0b0010_0000;
// const CONTROL_FLAG_SLAVE_MODE: u8 = 0b0100_0000;
const CONTROL_FLAG_NMI_ENABLED: u8 = 0b1000_0000;

pub struct ControlRegister {
    pub register: u8,
}

impl ControlRegister {
    pub fn new() -> Self {
        Self { register: 0 }
    }
    pub fn increment_mode(&self) -> bool {
        check_flag(self.register, CONTROL_FLAG_INC_MODE)
    }
    pub fn nametable_x(&self) -> bool {
        check_flag(self.register, CONTROL_FLAG_NAMETABLE_X)
    }
    pub fn nametable_y(&self) -> bool {
        check_flag(self.register, CONTROL_FLAG_NAMETABLE_Y)
    }
    pub fn nmi_enabled(&self) -> bool {
        check_flag(self.register, CONTROL_FLAG_NMI_ENABLED)
    }
}

const MASK_FLAG_GRAYSCALE: u8 = 0b0000_0001;
pub struct MaskRegister {
    pub register: u8,
}

impl MaskRegister {
    pub fn new() -> Self {
        Self { register: 0 }
    }
    pub fn gray_scale(&self) -> bool {
        check_flag(self.register, MASK_FLAG_GRAYSCALE)
    }
}

#[repr(u8)]
enum StatusFlagMask {
    SpriteOverflow = 0b0010_0000,
    SpriteZeroHit = 0b0100_0000,
    VerticalBlank = 0b1000_0000,
}
pub struct StatusRegister {
    pub register: u8,
}

impl StatusRegister {
    pub fn new() -> Self {
        Self { register: 0 }
    }
    pub fn set_vertical_blank(&mut self, status: bool) {
        if status {
            set_flag(&mut self.register, StatusFlagMask::VerticalBlank as u8);
        } else {
            clear_flag(&mut self.register, StatusFlagMask::VerticalBlank as u8);
        }
    }
    pub fn sprite_overflow(&self) -> bool {
        check_flag(self.register, StatusFlagMask::SpriteOverflow as u8)
    }
    pub fn sprite_zero_hit(&self) -> bool {
        check_flag(self.register, StatusFlagMask::SpriteZeroHit as u8)
    }
    pub fn set_sprite_zero_hit(&mut self, status: bool) {
        if status {
            set_flag(&mut self.register, StatusFlagMask::SpriteZeroHit as u8);
        } else {
            clear_flag(&mut self.register, StatusFlagMask::SpriteZeroHit as u8);
        }
    }
    pub fn vertical_blank(&self) -> bool {
        check_flag(self.register, StatusFlagMask::VerticalBlank as u8)
    }
}

pub struct LoopyRegister {
    bits: BitReg<u16>,
}

impl LoopyRegister {
    pub fn new() -> Self {
        Self {
            bits: BitReg::new(0),
        }
    }

    /*
    yyy NN YYYYY XXXXX
    ||| || ||||| +++++-- coarse X scroll
    ||| || +++++-------- coarse Y scroll
    ||| ++-------------- nametable select
    +++----------------- fine Y scroll
    */
    pub fn register(&self) -> u16 {
        self.bits.reg & 0x7FFF
    }
    pub fn set_register(&mut self, value: u16) {
        self.bits.reg = value;
    }

    pub fn set_coarse_x(&mut self, value: u8) {
        self.bits.set_bits(4, 0, value as u16);
    }
    pub fn set_coarse_y(&mut self, value: u8) {
        self.bits.set_bits(9, 5, value as u16);
    }
    pub fn set_nametable_x(&mut self, value: bool) {
        self.bits.set_bits(10, 10, if value { 1 } else { 0 });
    }
    pub fn set_nametable_y(&mut self, value: bool) {
        self.bits.set_bits(11, 11, if value { 1 } else { 0 });
    }
    pub fn set_fine_y(&mut self, value: u8) {
        self.bits.set_bits(14, 12, value as u16);
    }
}

#[inline]
fn clear_flag(reg: &mut u8, flag: u8) {
    *reg &= !flag;
}

#[inline]
fn set_flag(reg: &mut u8, flag: u8) {
    *reg |= flag;
}

#[inline]
fn check_flag(reg: u8, flag: u8) -> bool {
    reg & flag != 0
}
