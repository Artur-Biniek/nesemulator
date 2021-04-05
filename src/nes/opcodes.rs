pub const BRK: u8 = 0x00;

// LDA (LoaD Accumulator)
pub const LDA_IMM: u8 = 0xA9;
pub const LDA_ZP: u8 = 0xA5;
pub const LDA_ZP_X: u8 = 0xB5;
pub const LDA_ABS: u8 = 0xAD;
pub const LDA_ABS_X: u8 = 0xBD;
pub const LDA_ABS_Y: u8 = 0xB9;
pub const LDA_IND_X: u8 = 0xA1;
pub const LDA_IND_Y: u8 = 0xB1;

// LDX (LoaD X register)
pub const LDX_IMM: u8 = 0xA2;
pub const LDX_ZP: u8 = 0xA6;
pub const LDX_ZP_Y: u8 = 0xB6;
pub const LDX_ABS: u8 = 0xAE;
pub const LDX_ABS_Y: u8 = 0xBE;

pub enum AddressMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
}
