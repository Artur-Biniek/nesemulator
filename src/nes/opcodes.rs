pub const BRK: u8 = 0x00;

// LDA (LoaD Accumulator register)
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

// LDY (LoaD Y register)
pub const LDY_IMM: u8 = 0xA0;
pub const LDY_ZP: u8 = 0xA4;
pub const LDY_ZP_X: u8 = 0xB4;
pub const LDY_ABS: u8 = 0xAC;
pub const LDY_ABS_X: u8 = 0xBC;

// Register Instructions
pub const TAX: u8 = 0xAA;
pub const TXA: u8 = 0x8A;
pub const DEX: u8 = 0xCA;
pub const INX: u8 = 0xE8;

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

pub struct Instruction<'a> {
    pub mnemonic: &'a str,

    pub opcode: u8,

    pub addressing_mode: AddressMode,

    pub cycle_cost: u8,
}
