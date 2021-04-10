use lazy_static::lazy_static;
use std::collections::HashMap;

// Movee commands
pub const LDA_IMM: u8 = 0xA9;
pub const LDA_ZP: u8 = 0xA5;
pub const LDA_ZPX: u8 = 0xB5;
pub const LDA_IZX: u8 = 0xA1;
pub const LDA_IZY: u8 = 0xB1;
pub const LDA_ABS: u8 = 0xAD;
pub const LDA_ABX: u8 = 0xBD;
pub const LDA_ABY: u8 = 0xB9;

pub const STA_ZP: u8 = 0x85;
pub const STA_ZPX: u8 = 0x95;
pub const STA_IZX: u8 = 0x81;
pub const STA_IZY: u8 = 0x91;
pub const STA_ABS: u8 = 0x8D;
pub const STA_ABX: u8 = 0x9D;
pub const STA_ABY: u8 = 0x99;

pub const LDX_IMM: u8 = 0xA2;
pub const LDX_ZP: u8 = 0xA6;
pub const LDX_ZPY: u8 = 0xB6;
pub const LDX_ABS: u8 = 0xAE;
pub const LDX_ABY: u8 = 0xBE;

pub const STX_ZP: u8 = 0x86;
pub const STX_ZPY: u8 = 0x96;
pub const STX_ABS: u8 = 0x8E;

pub const LDY_IMM: u8 = 0xA0;
pub const LDY_ZP: u8 = 0xA4;
pub const LDY_ZPX: u8 = 0xB4;
pub const LDY_ABS: u8 = 0xAC;
pub const LDY_ABX: u8 = 0xBC;

pub const STY_ZP: u8 = 0x84;
pub const STY_ZPX: u8 = 0x94;
pub const STY_ABS: u8 = 0x8C;

pub const TAX_IMP: u8 = 0xAA;
pub const TXA_IMP: u8 = 0x8A;
pub const TAY_IMP: u8 = 0xA8;
pub const TYA_IMP: u8 = 0x98;
pub const TSX_IMP: u8 = 0xBA;
pub const TXS_IMP: u8 = 0x9A;
pub const PLA_IMP: u8 = 0x68;
pub const PHA_IMP: u8 = 0x48;
pub const PLP_IMP: u8 = 0x28;
pub const PHP_IMP: u8 = 0x08;

// Logical and arithmetic commands
pub const ORA_IMM: u8 = 0x09;
pub const ORA_ZP: u8 = 0x05;
pub const ORA_ZPX: u8 = 0x15;
pub const ORA_IZX: u8 = 0x01;
pub const ORA_IZY: u8 = 0x11;
pub const ORA_ABS: u8 = 0x0D;
pub const ORA_ABX: u8 = 0x1D;
pub const ORA_ABY: u8 = 0x19;

pub const AND_IMM: u8 = 0x29;
pub const AND_ZP: u8 = 0x25;
pub const AND_ZPX: u8 = 0x35;
pub const AND_IZX: u8 = 0x21;
pub const AND_IZY: u8 = 0x31;
pub const AND_ABS: u8 = 0x2D;
pub const AND_ABX: u8 = 0x3D;
pub const AND_ABY: u8 = 0x39;

pub const EOR_IMM: u8 = 0x49;
pub const EOR_ZP: u8 = 0x45;
pub const EOR_ZPX: u8 = 0x55;
pub const EOR_IZX: u8 = 0x41;
pub const EOR_IZY: u8 = 0x51;
pub const EOR_ABS: u8 = 0x4D;
pub const EOR_ABX: u8 = 0x5D;
pub const EOR_ABY: u8 = 0x59;

pub const ADC_IMM: u8 = 0x69;
pub const ADC_ZP: u8 = 0x65;
pub const ADC_ZPX: u8 = 0x75;
pub const ADC_IZX: u8 = 0x61;
pub const ADC_IZY: u8 = 0x71;
pub const ADC_ABS: u8 = 0x6D;
pub const ADC_ABX: u8 = 0x7D;
pub const ADC_ABY: u8 = 0x79;

pub const SBC_IMM: u8 = 0xE9;
pub const SBC_ZP: u8 = 0xE5;
pub const SBC_ZPX: u8 = 0xF5;
pub const SBC_IZX: u8 = 0xE1;
pub const SBC_IZY: u8 = 0xF1;
pub const SBC_ABS: u8 = 0xED;
pub const SBC_ABX: u8 = 0xFD;
pub const SBC_ABY: u8 = 0xF9;

pub const CMP_IMM: u8 = 0xC9;
pub const CMP_ZP: u8 = 0xC5;
pub const CMP_ZPX: u8 = 0xD5;
pub const CMP_IZX: u8 = 0xC1;
pub const CMP_IZY: u8 = 0xD1;
pub const CMP_ABS: u8 = 0xCD;
pub const CMP_ABX: u8 = 0xDD;
pub const CMP_ABY: u8 = 0xD9;

pub const CPX_IMM: u8 = 0xE0;
pub const CPX_ZP: u8 = 0xE4;
pub const CPX_ABS: u8 = 0xEC;

pub const CPY_IMM: u8 = 0xC0;
pub const CPY_ZP: u8 = 0xC4;
pub const CPY_ABS: u8 = 0xCC;

pub const DEC_ZP: u8 = 0xC6;
pub const DEC_ZPX: u8 = 0xD6;
pub const DEC_ABS: u8 = 0xCE;
pub const DEC_ABX: u8 = 0xDE;

pub const INC_ZP: u8 = 0xE6;
pub const INC_ZPX: u8 = 0xF6;
pub const INC_ABS: u8 = 0xEE;
pub const INC_ABX: u8 = 0xFE;

pub const DEX_IMP: u8 = 0xCA;
pub const INX_IMP: u8 = 0xE8;

pub const DEY_IMP: u8 = 0x88;
pub const INY_IMP: u8 = 0xC8;

pub const ASL_IMP: u8 = 0x0A;
pub const ASL_ZP: u8 = 0x06;
pub const ASL_ZPX: u8 = 0x16;
pub const ASL_ABS: u8 = 0x0E;
pub const ASL_ABX: u8 = 0x1E;

pub const ROL_IMP: u8 = 0x2A;
pub const ROL_ZP: u8 = 0x26;
pub const ROL_ZPX: u8 = 0x36;
pub const ROL_ABS: u8 = 0x2E;
pub const ROL_ABX: u8 = 0x3E;

pub const LSR_IMP: u8 = 0x4A;
pub const LSR_ZP: u8 = 0x46;
pub const LSR_ZPX: u8 = 0x56;
pub const LSR_ABS: u8 = 0x4E;
pub const LSR_ABX: u8 = 0x5E;

pub const ROR_IMP: u8 = 0x6A;
pub const ROR_ZP: u8 = 0x66;
pub const ROR_ZPX: u8 = 0x76;
pub const ROR_ABS: u8 = 0x6E;
pub const ROR_ABX: u8 = 0x7E;

// Jump/Flag commands
pub const BPL_REL: u8 = 0x10;
pub const BMI_REL: u8 = 0x30;
pub const BVC_REL: u8 = 0x50;
pub const BVS_REL: u8 = 0x70;
pub const BCC_REL: u8 = 0x90;
pub const BCS_REL: u8 = 0xB0;
pub const BNE_REL: u8 = 0xD0;
pub const BEQ_REL: u8 = 0xF0;

pub const BRK_IMP: u8 = 0x00;
pub const RTI_IMP: u8 = 0x40;
pub const JSR_ABS: u8 = 0x20;
pub const RTS_IMP: u8 = 0x60;

pub const JMP_ABS: u8 = 0x4C;
pub const JMP_IND: u8 = 0x6C;

pub const BIT_ZP: u8 = 0x24;
pub const BIT_ABS: u8 = 0x2C;

pub const CLC_IMP: u8 = 0x18;
pub const SEC_IMP: u8 = 0x38;
pub const CLD_IMP: u8 = 0xD8;
pub const SED_IMP: u8 = 0xF8;
pub const CLI_IMP: u8 = 0x58;
pub const SEI_IMP: u8 = 0x78;
pub const CLV_IMP: u8 = 0xB8;

pub const NOP_IMP: u8 = 0xEA;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AddressMode {
    None,
    Implied,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    IndirectZeroX,
    IndirectZeroY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    Relative,
}

pub struct Instruction<'a> {
    pub mnemonic: &'a str,
    pub opcode: u8,
    pub addressing_mode: AddressMode,
    pub cycle_cost: u8,
}

impl<'a> Instruction<'a> {
    pub fn new(
        opcode: u8,
        mnemonic: &'a str,
        addressing_mode: AddressMode,
        cycle_cost: u8,
    ) -> Self {
        Self {
            mnemonic,
            opcode,
            addressing_mode,
            cycle_cost,
        }
    }

    pub fn ill(opcode: u8) -> Self {
        Self {
            mnemonic: "???",
            opcode,
            addressing_mode: AddressMode::None,
            cycle_cost: 0,
        }
    }
}

lazy_static! {
    static ref INSTRUCTIONS: HashMap<u8, Instruction<'static>> = {
        let mut m = HashMap::new();

        let v = vec![
            // 0x0_
            Instruction::new(0x00, "BRK", AddressMode::Implied, 7),
            Instruction::new(0x01, "ORA", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0x02),
            Instruction::ill(0x03),
            Instruction::ill(0x04),
            Instruction::new(0x05, "ORA", AddressMode::ZeroPage, 3),
            Instruction::new(0x06, "ASL", AddressMode::ZeroPage, 5),
            Instruction::ill(0x07),
            Instruction::new(0x08, "PHP", AddressMode::Implied, 3),
            Instruction::new(0x09, "ORA", AddressMode::Immediate, 2),
            Instruction::new(0x0A, "ASL", AddressMode::Implied, 2),
            Instruction::ill(0x0B),
            Instruction::ill(0x0C),
            Instruction::new(0x0D, "ORA", AddressMode::Absolute, 4),
            Instruction::new(0x0E, "ASL", AddressMode::Absolute, 6),
            Instruction::ill(0x0F),
            // 0x1_
            Instruction::new(0x10, "BPL", AddressMode::Relative, 2),
            Instruction::new(0x11, "ORA", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0x12),
            Instruction::ill(0x13),
            Instruction::ill(0x14),
            Instruction::new(0x15, "ORA", AddressMode::ZeroPageX, 4),
            Instruction::new(0x16, "ASL", AddressMode::ZeroPageX, 6),
            Instruction::ill(0x17),
            Instruction::new(0x18, "CLC", AddressMode::Implied, 2),
            Instruction::new(0x19, "ORA", AddressMode::AbsoluteY, 4),
            Instruction::ill(0x1A),
            Instruction::ill(0x1B),
            Instruction::ill(0x1C),
            Instruction::new(0x1D, "ORA", AddressMode::AbsoluteX, 4),
            Instruction::new(0x1E, "ASL", AddressMode::AbsoluteX, 7),
            Instruction::ill(0x1F),

            // 0x2_
            Instruction::new(0x20, "JSR", AddressMode::Absolute, 6),
            Instruction::new(0x21, "AND", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0x22),
            Instruction::ill(0x23),
            Instruction::new(0x24, "BIT", AddressMode::ZeroPage, 3),
            Instruction::new(0x25, "AND", AddressMode::ZeroPage, 3),
            Instruction::new(0x26, "ROL", AddressMode::ZeroPage, 5),
            Instruction::ill(0x27),
            Instruction::new(0x28, "PLP", AddressMode::Implied, 4),
            Instruction::new(0x29, "AND", AddressMode::Immediate, 2),
            Instruction::new(0x2A, "ROL", AddressMode::Implied, 2),
            Instruction::ill(0x2B),
            Instruction::new(0x2C, "BIT", AddressMode::Absolute, 4),
            Instruction::new(0x2D, "AND", AddressMode::Absolute, 4),
            Instruction::new(0x2E, "ROL", AddressMode::Absolute, 6),
            Instruction::ill(0x2F),

            // 0x3_
            Instruction::new(0x30, "BMI", AddressMode::Relative, 2),
            Instruction::new(0x31, "AND", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0x32),
            Instruction::ill(0x33),
            Instruction::ill(0x34),
            Instruction::new(0x35, "AND", AddressMode::ZeroPageX, 4),
            Instruction::new(0x36, "ROL", AddressMode::ZeroPageX, 6),
            Instruction::ill(0x37),
            Instruction::new(0x38, "SEC", AddressMode::Implied, 2),
            Instruction::new(0x39, "AND", AddressMode::AbsoluteY, 4),
            Instruction::ill(0x3A),
            Instruction::ill(0x3B),
            Instruction::ill(0x3C),
            Instruction::new(0x3D, "AND", AddressMode::AbsoluteX, 4),
            Instruction::new(0x3E, "ROL", AddressMode::AbsoluteX, 7),
            Instruction::ill(0x3F),
            // 0x4_
            Instruction::new(0x40, "RTI", AddressMode::Implied, 6),
            Instruction::new(0x41, "EOR", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0x42),
            Instruction::ill(0x43),
            Instruction::ill(0x44),
            Instruction::new(0x45, "EOR", AddressMode::ZeroPage, 3),
            Instruction::new(0x46, "LSR", AddressMode::ZeroPage, 5),
            Instruction::ill(0x47),
            Instruction::new(0x48, "PHA", AddressMode::Implied, 3),
            Instruction::new(0x49, "EOR", AddressMode::Immediate, 2),
            Instruction::new(0x4A, "LSR", AddressMode::Implied, 2),
            Instruction::ill(0x4B),
            Instruction::new(0x4C, "JMP", AddressMode::Absolute, 3),
            Instruction::new(0x4D, "EOR", AddressMode::Absolute, 4),
            Instruction::new(0x4E, "LSR", AddressMode::Absolute, 6),
            Instruction::ill(0x4F),
            // 0x5_
            Instruction::new(0x50, "BVC", AddressMode::Relative, 2),
            Instruction::new(0x51, "EOR", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0x52),
            Instruction::ill(0x53),
            Instruction::ill(0x54),
            Instruction::new(0x55, "EOR", AddressMode::ZeroPageX, 4),
            Instruction::new(0x56, "LSR", AddressMode::ZeroPageX, 6),
            Instruction::ill(0x57),
            Instruction::new(0x58, "CLI", AddressMode::Implied, 2),
            Instruction::new(0x59, "EOR", AddressMode::AbsoluteY, 4),
            Instruction::ill(0x5A),
            Instruction::ill(0x5B),
            Instruction::ill(0x5C),
            Instruction::new(0x5D, "EOR", AddressMode::AbsoluteX, 4),
            Instruction::new(0x5E, "LSR", AddressMode::AbsoluteX, 7),
            Instruction::ill(0x5F),
            // 0x6_
            Instruction::new(0x60, "RTS", AddressMode::Implied, 6),
            Instruction::new(0x61, "ADC", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0x62),
            Instruction::ill(0x63),
            Instruction::ill(0x64),
            Instruction::new(0x65, "ADC", AddressMode::ZeroPage, 3),
            Instruction::new(0x66, "ROR", AddressMode::ZeroPage, 5),
            Instruction::ill(0x67),
            Instruction::new(0x68, "PLA", AddressMode::Implied, 4),
            Instruction::new(0x69, "ADC", AddressMode::Immediate, 2),
            Instruction::new(0x6A, "ROR", AddressMode::Implied, 2),
            Instruction::ill(0x6B),
            Instruction::new(0x6C, "JMP", AddressMode::Indirect, 5),
            Instruction::new(0x6D, "ADC", AddressMode::Absolute, 4),
            Instruction::new(0x6E, "ROR", AddressMode::Absolute, 6),
            Instruction::ill(0x6F),

            // 0x7_
            Instruction::new(0x70, "BVS", AddressMode::Relative, 2),
            Instruction::new(0x71, "ADC", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0x72),
            Instruction::ill(0x73),
            Instruction::ill(0x74),
            Instruction::new(0x75, "ADC", AddressMode::ZeroPageX, 4),
            Instruction::new(0x76, "ROR", AddressMode::ZeroPageX, 6),
            Instruction::ill(0x77),
            Instruction::new(0x78, "SEI", AddressMode::Implied, 2),
            Instruction::new(0x79, "ADC", AddressMode::AbsoluteY, 4),
            Instruction::ill(0x7A),
            Instruction::ill(0x7B),
            Instruction::ill(0x7C),
            Instruction::new(0x7D, "ADC", AddressMode::AbsoluteX, 4),
            Instruction::new(0x7E, "ROR", AddressMode::AbsoluteX, 7),
            Instruction::ill(0x7F),

            // 0x8_
            Instruction::ill(0x80),
            Instruction::new(0x81, "STA", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0x82),
            Instruction::ill(0x83),
            Instruction::new(0x84, "STY", AddressMode::ZeroPage, 3),
            Instruction::new(0x85, "STA", AddressMode::ZeroPage, 3),
            Instruction::new(0x86, "STX", AddressMode::ZeroPage, 3),
            Instruction::ill(0x87),
            Instruction::new(0x88, "DEY", AddressMode::Implied, 2),
            Instruction::ill(0x89),
            Instruction::new(0x8A, "TXA", AddressMode::Implied, 2),
            Instruction::ill(0x8B),
            Instruction::new(0x8C, "STY", AddressMode::Absolute, 4),
            Instruction::new(0x8D, "STA", AddressMode::Absolute, 4),
            Instruction::new(0x8E, "STX", AddressMode::Absolute, 4),
            Instruction::ill(0x8F),

            // 0x9_
            Instruction::new(0x90, "BCC", AddressMode::Relative, 2),
            Instruction::new(0x91, "STA", AddressMode::IndirectZeroY, 6),
            Instruction::ill(0x92),
            Instruction::ill(0x93),
            Instruction::new(0x94, "STY", AddressMode::ZeroPageX, 4),
            Instruction::new(0x95, "STA", AddressMode::ZeroPageX, 4),
            Instruction::new(0x96, "STX", AddressMode::ZeroPageY, 4),
            Instruction::ill(0x97),
            Instruction::new(0x98, "TYA", AddressMode::Implied, 2),
            Instruction::new(0x99, "STA", AddressMode::AbsoluteY, 5),
            Instruction::new(0x9A, "TXS", AddressMode::Implied, 2),
            Instruction::ill(0x9B),
            Instruction::ill(0x9C),
            Instruction::new(0x9D, "STA", AddressMode::AbsoluteX, 5),
            Instruction::ill(0x9E),
            Instruction::ill(0x9F),

            // 0xA_
            Instruction::new(0xA0, "LDY", AddressMode::Immediate, 2),
            Instruction::new(0xA1, "LDA", AddressMode::IndirectZeroX, 6),
            Instruction::new(0xA2, "LDX", AddressMode::Immediate, 2),
            Instruction::ill(0xA3),
            Instruction::new(0xA4, "LDY", AddressMode::ZeroPage, 3),
            Instruction::new(0xA5, "LDA", AddressMode::ZeroPage, 3),
            Instruction::new(0xA6, "LDX", AddressMode::ZeroPage, 3),
            Instruction::ill(0xA7),
            Instruction::new(0xA8, "TAY", AddressMode::Implied, 2),
            Instruction::new(0xA9, "LDA", AddressMode::Immediate, 2),
            Instruction::new(0xAA, "TAX", AddressMode::Implied, 2),
            Instruction::ill(0xAB),
            Instruction::new(0xAC, "LDY", AddressMode::Absolute, 4),
            Instruction::new(0xAD, "LDA", AddressMode::Absolute, 4),
            Instruction::new(0xAE, "LDX", AddressMode::Absolute, 4),
            Instruction::ill(0xAF),

            // 0xB_
            Instruction::new(0xB0, "BCS", AddressMode::Relative, 2),
            Instruction::new(0xB1, "LDA", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0xB2),
            Instruction::ill(0xB3),
            Instruction::new(0xB4, "LDY", AddressMode::ZeroPageX, 4),
            Instruction::new(0xB5, "LDA", AddressMode::ZeroPageX, 4),
            Instruction::new(0xB6, "LDX", AddressMode::ZeroPageY, 4),
            Instruction::ill(0xB7),
            Instruction::new(0xB8, "CLV", AddressMode::Implied, 2),
            Instruction::new(0xB9, "LDA", AddressMode::AbsoluteY, 4),
            Instruction::new(0xBA, "TSX", AddressMode::Implied, 2),
            Instruction::ill(0xBB),
            Instruction::new(0xBC, "LDY", AddressMode::AbsoluteX, 4),
            Instruction::new(0xBD, "LDA", AddressMode::AbsoluteX, 4),
            Instruction::new(0xBE, "LDX", AddressMode::AbsoluteY, 4),
            Instruction::ill(0xBF),

            // 0xC_
            Instruction::new(0xC0, "CPY", AddressMode::Immediate, 2),
            Instruction::new(0xC1, "CMP", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0xC2),
            Instruction::ill(0xC3),
            Instruction::new(0xC4, "CPY", AddressMode::ZeroPage, 3),
            Instruction::new(0xC5, "CMP", AddressMode::ZeroPage, 3),
            Instruction::new(0xC6, "DEC", AddressMode::ZeroPage, 5),
            Instruction::ill(0xC7),
            Instruction::new(0xC8, "INY", AddressMode::Implied, 2),
            Instruction::new(0xC9, "CMP", AddressMode::Immediate, 2),
            Instruction::new(0xCA, "DEX", AddressMode::Implied, 2),
            Instruction::ill(0xCB),
            Instruction::new(0xCC, "CPY", AddressMode::Absolute, 4),
            Instruction::new(0xCD, "CMP", AddressMode::Absolute, 4),
            Instruction::new(0xCE, "DEC", AddressMode::Absolute, 6),
            Instruction::ill(0xCF),

            // 0xD_
            Instruction::new(0xD0, "BNE", AddressMode::Relative, 2),
            Instruction::new(0xD1, "CMP", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0xD2),
            Instruction::ill(0xD3),
            Instruction::ill(0xD4),
            Instruction::new(0xD5, "CMP", AddressMode::ZeroPageX, 4),
            Instruction::new(0xD6, "DEC", AddressMode::ZeroPageX, 6),
            Instruction::ill(0xD7),
            Instruction::new(0xD8, "CLD", AddressMode::Implied, 2),
            Instruction::new(0xD9, "CMP", AddressMode::AbsoluteY, 4),
            Instruction::ill(0xDA),
            Instruction::ill(0xDB),
            Instruction::ill(0xDC),
            Instruction::new(0xDD, "CMP", AddressMode::AbsoluteX, 4),
            Instruction::new(0xDE, "DEC", AddressMode::AbsoluteX, 7),
            Instruction::ill(0xDF),

            // 0xE_
            Instruction::new(0xE0, "CPX", AddressMode::Immediate, 2),
            Instruction::new(0xE1, "SBC", AddressMode::IndirectZeroX, 6),
            Instruction::ill(0xE2),
            Instruction::ill(0xE3),
            Instruction::new(0xE4, "CPX", AddressMode::ZeroPage, 3),
            Instruction::new(0xE5, "SBC", AddressMode::ZeroPage, 3),
            Instruction::new(0xE6, "INC", AddressMode::ZeroPage, 5),
            Instruction::ill(0xE7),
            Instruction::new(0xE8, "INX", AddressMode::Implied, 2),
            Instruction::new(0xE9, "SBC", AddressMode::Immediate, 2),
            Instruction::new(0xEA, "NOP", AddressMode::Implied, 2),
            Instruction::ill(0xEB),
            Instruction::new(0xEC, "CPX", AddressMode::Absolute, 4),
            Instruction::new(0xED, "SBC", AddressMode::Absolute, 4),
            Instruction::new(0xEE, "INC", AddressMode::Absolute, 6),
            Instruction::ill(0xEF),

            // 0xF_
            Instruction::new(0xF0, "BEQ", AddressMode::Relative, 2),
            Instruction::new(0xF1, "SBC", AddressMode::IndirectZeroY, 5),
            Instruction::ill(0xF2),
            Instruction::ill(0xF3),
            Instruction::ill(0xF4),
            Instruction::new(0xF5, "SBC", AddressMode::ZeroPageX, 4),
            Instruction::new(0xF6, "INC", AddressMode::ZeroPageX, 6),
            Instruction::ill(0xF7),
            Instruction::new(0xF8, "SED", AddressMode::Implied, 2),
            Instruction::new(0xF9, "SBC", AddressMode::AbsoluteY, 4),
            Instruction::ill(0xFA),
            Instruction::ill(0xFB),
            Instruction::ill(0xFC),
            Instruction::new(0xFD, "SBC", AddressMode::AbsoluteX, 4),
            Instruction::new(0xFE, "INC", AddressMode::AbsoluteX, 7),
            Instruction::ill(0xFF),
        ];

        for ins in v {
            m.insert(ins.opcode, ins);
        }

        m
    };
}

pub fn get_inst(opcode: u8) -> &'static Instruction<'static> {
    INSTRUCTIONS.get(&opcode).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn brk() {
        let brk = get_inst(0x00);
        assert_eq!(brk.mnemonic, "BRK");
        assert_eq!(brk.addressing_mode, AddressMode::Implied);
        assert_eq!(brk.cycle_cost, 7);
    }

    #[test]
    pub fn ora_ind_x() {
        let ins = get_inst(0x01);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_02() {
        let ins = get_inst(0x02);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }
    #[test]
    pub fn ill_03() {
        let ins = get_inst(0x03);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_04() {
        let ins = get_inst(0x04);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ora_zp() {
        let ins = get_inst(0x05);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn asl_zp() {
        let ins = get_inst(0x06);
        assert_eq!(ins.mnemonic, "ASL");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_07() {
        let ins = get_inst(0x07);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn php_imp() {
        let ins = get_inst(0x08);
        assert_eq!(ins.mnemonic, "PHP");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn ora_imm() {
        let ins = get_inst(0x09);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn asl_acc() {
        let ins = get_inst(0x0A);
        assert_eq!(ins.mnemonic, "ASL");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }
    #[test]
    pub fn ill_0b() {
        let ins = get_inst(0x0B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_xc() {
        let ins = get_inst(0x0C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ora_abs() {
        let ins = get_inst(0x0D);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn asl_abs() {
        let ins = get_inst(0x0E);
        assert_eq!(ins.mnemonic, "ASL");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_0f() {
        let ins = get_inst(0x0F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bpl_rel() {
        let ins = get_inst(0x10);
        assert_eq!(ins.mnemonic, "BPL");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ora_ind_y() {
        let ins = get_inst(0x11);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_12() {
        let ins = get_inst(0x12);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_13() {
        let ins = get_inst(0x13);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_14() {
        let ins = get_inst(0x14);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ora_zpx() {
        let ins = get_inst(0x15);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn asl_zpx() {
        let ins = get_inst(0x16);
        assert_eq!(ins.mnemonic, "ASL");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_17() {
        let ins = get_inst(0x17);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn clc_imp() {
        let ins = get_inst(0x18);
        assert_eq!(ins.mnemonic, "CLC");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ora_abs_y() {
        let ins = get_inst(0x19);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_1a() {
        let ins = get_inst(0x1A);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_1b() {
        let ins = get_inst(0x1B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_1c() {
        let ins = get_inst(0x1C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ora_abs_x() {
        let ins = get_inst(0x1D);
        assert_eq!(ins.mnemonic, "ORA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn asl_abs_x() {
        let ins = get_inst(0x1E);
        assert_eq!(ins.mnemonic, "ASL");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn ill_1f() {
        let ins = get_inst(0x1F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn jsr_abs() {
        let ins = get_inst(0x20);
        assert_eq!(ins.mnemonic, "JSR");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn and_ind_x() {
        let ins = get_inst(0x21);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_22() {
        let ins = get_inst(0x22);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_23() {
        let ins = get_inst(0x23);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bit_zp() {
        let ins = get_inst(0x24);
        assert_eq!(ins.mnemonic, "BIT");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn and_zp() {
        let ins = get_inst(0x25);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn rol_zp() {
        let ins = get_inst(0x26);
        assert_eq!(ins.mnemonic, "ROL");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_27() {
        let ins = get_inst(0x27);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn plp_imp() {
        let ins = get_inst(0x28);
        assert_eq!(ins.mnemonic, "PLP");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn and_imm() {
        let ins = get_inst(0x29);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn rol_imp() {
        let ins = get_inst(0x2A);
        assert_eq!(ins.mnemonic, "ROL");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_2b() {
        let ins = get_inst(0x2B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bit_abs() {
        let ins = get_inst(0x2C);
        assert_eq!(ins.mnemonic, "BIT");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn and_abs() {
        let ins = get_inst(0x2D);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn rol_abs() {
        let ins = get_inst(0x2E);
        assert_eq!(ins.mnemonic, "ROL");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_2f() {
        let ins = get_inst(0x2F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bmi_rel() {
        let ins = get_inst(0x30);
        assert_eq!(ins.mnemonic, "BMI");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn and_ind_y() {
        let ins = get_inst(0x31);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_32() {
        let ins = get_inst(0x32);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_33() {
        let ins = get_inst(0x33);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_34() {
        let ins = get_inst(0x34);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn and_zpx() {
        let ins = get_inst(0x35);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn rol_zpx() {
        let ins = get_inst(0x36);
        assert_eq!(ins.mnemonic, "ROL");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_37() {
        let ins = get_inst(0x37);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sec_imp() {
        let ins = get_inst(0x38);
        assert_eq!(ins.mnemonic, "SEC");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn and_abs_y() {
        let ins = get_inst(0x39);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_3a() {
        let ins = get_inst(0x3A);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_3b() {
        let ins = get_inst(0x3B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_3c() {
        let ins = get_inst(0x3C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn and_abs_x() {
        let ins = get_inst(0x3D);
        assert_eq!(ins.mnemonic, "AND");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn rol_abs_x() {
        let ins = get_inst(0x3E);
        assert_eq!(ins.mnemonic, "ROL");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn ill_3f() {
        let ins = get_inst(0x3F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn rti_imp() {
        let ins = get_inst(0x40);
        assert_eq!(ins.mnemonic, "RTI");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn eor_ind_x() {
        let ins = get_inst(0x41);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_42() {
        let ins = get_inst(0x42);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_43() {
        let ins = get_inst(0x43);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_44() {
        let ins = get_inst(0x44);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn eor_zp() {
        let ins = get_inst(0x45);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn lsr_zp() {
        let ins = get_inst(0x46);
        assert_eq!(ins.mnemonic, "LSR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_47() {
        let ins = get_inst(0x47);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn plh_imp() {
        let ins = get_inst(0x48);
        assert_eq!(ins.mnemonic, "PHA");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn eor_imm() {
        let ins = get_inst(0x49);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn lsr_imp() {
        let ins = get_inst(0x4A);
        assert_eq!(ins.mnemonic, "LSR");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_4b() {
        let ins = get_inst(0x4B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn jmp_abs() {
        let ins = get_inst(0x4C);
        assert_eq!(ins.mnemonic, "JMP");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn eor_abs() {
        let ins = get_inst(0x4D);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lsr_abs() {
        let ins = get_inst(0x4E);
        assert_eq!(ins.mnemonic, "LSR");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_4f() {
        let ins = get_inst(0x4F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bvc_rel() {
        let ins = get_inst(0x50);
        assert_eq!(ins.mnemonic, "BVC");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn eor_ind_y() {
        let ins = get_inst(0x51);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_52() {
        let ins = get_inst(0x52);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_53() {
        let ins = get_inst(0x53);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_54() {
        let ins = get_inst(0x54);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn eor_zpx() {
        let ins = get_inst(0x55);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lsr_zpx() {
        let ins = get_inst(0x56);
        assert_eq!(ins.mnemonic, "LSR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_57() {
        let ins = get_inst(0x57);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cli_imp() {
        let ins = get_inst(0x58);
        assert_eq!(ins.mnemonic, "CLI");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn eor_abs_y() {
        let ins = get_inst(0x59);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_5a() {
        let ins = get_inst(0x5A);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_5b() {
        let ins = get_inst(0x5B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_5c() {
        let ins = get_inst(0x5C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn eor_abs_x() {
        let ins = get_inst(0x5D);
        assert_eq!(ins.mnemonic, "EOR");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lsr_abs_x() {
        let ins = get_inst(0x5E);
        assert_eq!(ins.mnemonic, "LSR");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn rtx_imp() {
        let ins = get_inst(0x60);
        assert_eq!(ins.mnemonic, "RTS");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn adc_ind_x() {
        let ins = get_inst(0x61);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_62() {
        let ins = get_inst(0x62);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_63() {
        let ins = get_inst(0x63);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_64() {
        let ins = get_inst(0x64);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn adc_zp() {
        let ins = get_inst(0x65);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn ror_zp() {
        let ins = get_inst(0x66);
        assert_eq!(ins.mnemonic, "ROR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_67() {
        let ins = get_inst(0x67);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn pla_imp() {
        let ins = get_inst(0x68);
        assert_eq!(ins.mnemonic, "PLA");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn adc_imm() {
        let ins = get_inst(0x69);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ror_imp() {
        let ins = get_inst(0x6A);
        assert_eq!(ins.mnemonic, "ROR");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_6b() {
        let ins = get_inst(0x6B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn jmp_ind() {
        let ins = get_inst(0x6C);
        assert_eq!(ins.mnemonic, "JMP");
        assert_eq!(ins.addressing_mode, AddressMode::Indirect);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn adc_abs() {
        let ins = get_inst(0x6D);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ror_abs() {
        let ins = get_inst(0x6E);
        assert_eq!(ins.mnemonic, "ROR");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_6f() {
        let ins = get_inst(0x6F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bvs_rel() {
        let ins = get_inst(0x70);
        assert_eq!(ins.mnemonic, "BVS");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn adc_ind_y() {
        let ins = get_inst(0x71);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_72() {
        let ins = get_inst(0x72);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_73() {
        let ins = get_inst(0x73);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_74() {
        let ins = get_inst(0x74);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn adc_zpx() {
        let ins = get_inst(0x75);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ror_zpx() {
        let ins = get_inst(0x76);
        assert_eq!(ins.mnemonic, "ROR");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_77() {
        let ins = get_inst(0x77);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sei_imp() {
        let ins = get_inst(0x78);
        assert_eq!(ins.mnemonic, "SEI");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn adc_abs_y() {
        let ins = get_inst(0x79);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_7a() {
        let ins = get_inst(0x7A);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_7b() {
        let ins = get_inst(0x7B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_7c() {
        let ins = get_inst(0x7C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn adc_abs_x() {
        let ins = get_inst(0x7D);
        assert_eq!(ins.mnemonic, "ADC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ror_abs_x() {
        let ins = get_inst(0x7E);
        assert_eq!(ins.mnemonic, "ROR");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn ill_80() {
        let ins = get_inst(0x80);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sta_ind_x() {
        let ins = get_inst(0x81);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_82() {
        let ins = get_inst(0x82);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_83() {
        let ins = get_inst(0x83);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sty_zp() {
        let ins = get_inst(0x84);
        assert_eq!(ins.mnemonic, "STY");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn sta_zp() {
        let ins = get_inst(0x85);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn stx_zp() {
        let ins = get_inst(0x86);
        assert_eq!(ins.mnemonic, "STX");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn ill_87() {
        let ins = get_inst(0x87);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn dey_imp() {
        let ins = get_inst(0x88);
        assert_eq!(ins.mnemonic, "DEY");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_89() {
        let ins = get_inst(0x89);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn txa_imp() {
        let ins = get_inst(0x8A);
        assert_eq!(ins.mnemonic, "TXA");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_8b() {
        let ins = get_inst(0x8B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sty_abs() {
        let ins = get_inst(0x8C);
        assert_eq!(ins.mnemonic, "STY");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn sta_abs() {
        let ins = get_inst(0x8D);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn stx_abs() {
        let ins = get_inst(0x8E);
        assert_eq!(ins.mnemonic, "STX");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_8f() {
        let ins = get_inst(0x8F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bcc_rel() {
        let ins = get_inst(0x90);
        assert_eq!(ins.mnemonic, "BCC");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn sta_ind_y() {
        let ins = get_inst(0x91);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_92() {
        let ins = get_inst(0x92);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_93() {
        let ins = get_inst(0x93);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sty_zpx() {
        let ins = get_inst(0x94);
        assert_eq!(ins.mnemonic, "STY");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn sta_zpx() {
        let ins = get_inst(0x95);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn stx_zpx() {
        let ins = get_inst(0x96);
        assert_eq!(ins.mnemonic, "STX");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_97() {
        let ins = get_inst(0x97);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn tya_imp() {
        let ins = get_inst(0x98);
        assert_eq!(ins.mnemonic, "TYA");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_99() {
        let ins = get_inst(0x99);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn txs_imp() {
        let ins = get_inst(0x9A);
        assert_eq!(ins.mnemonic, "TXS");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_9b() {
        let ins = get_inst(0x9B);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_9c() {
        let ins = get_inst(0x9C);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sta_abs_x() {
        let ins = get_inst(0x9D);
        assert_eq!(ins.mnemonic, "STA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_9e() {
        let ins = get_inst(0x9E);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_9f() {
        let ins = get_inst(0x9F);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ldy_imm() {
        let ins = get_inst(0xA0);
        assert_eq!(ins.mnemonic, "LDY");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn lda_ind_x() {
        let ins = get_inst(0xA1);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ldx_imm() {
        let ins = get_inst(0xa2);
        assert_eq!(ins.mnemonic, "LDX");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_a3() {
        let ins = get_inst(0xA3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ldy_zp() {
        let ins = get_inst(0xA4);
        assert_eq!(ins.mnemonic, "LDY");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn lda_zp() {
        let ins = get_inst(0xA5);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn ldx_zp() {
        let ins = get_inst(0xA6);
        assert_eq!(ins.mnemonic, "LDX");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn ill_a7() {
        let ins = get_inst(0xA7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn tay_imp() {
        let ins = get_inst(0xA8);
        assert_eq!(ins.mnemonic, "TAY");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn lda_imm() {
        let ins = get_inst(0xA9);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn tax_imp() {
        let ins = get_inst(0xAA);
        assert_eq!(ins.mnemonic, "TAX");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_ab() {
        let ins = get_inst(0xAB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ldy_abs() {
        let ins = get_inst(0xAC);
        assert_eq!(ins.mnemonic, "LDY");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lda_abs() {
        let ins = get_inst(0xAD);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ldx_abs() {
        let ins = get_inst(0xAE);
        assert_eq!(ins.mnemonic, "LDX");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_af() {
        let ins = get_inst(0xAF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bcs_rel() {
        let ins = get_inst(0xB0);
        assert_eq!(ins.mnemonic, "BCS");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn lda_ind_y() {
        let ins = get_inst(0xB1);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_b2() {
        let ins = get_inst(0xb2);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_b3() {
        let ins = get_inst(0xB3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ldy_zpx() {
        let ins = get_inst(0xB4);
        assert_eq!(ins.mnemonic, "LDY");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lda_zpx() {
        let ins = get_inst(0xB5);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ldx_zpx() {
        let ins = get_inst(0xB6);
        assert_eq!(ins.mnemonic, "LDX");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_b7() {
        let ins = get_inst(0xB7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn clv_imp() {
        let ins = get_inst(0xB8);
        assert_eq!(ins.mnemonic, "CLV");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn lda_abs_y() {
        let ins = get_inst(0xB9);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn tsx_imp() {
        let ins = get_inst(0xBA);
        assert_eq!(ins.mnemonic, "TSX");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_bb() {
        let ins = get_inst(0xBB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ldy_abs_x() {
        let ins = get_inst(0xBC);
        assert_eq!(ins.mnemonic, "LDY");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn lda_abs_x() {
        let ins = get_inst(0xBD);
        assert_eq!(ins.mnemonic, "LDA");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ldx_abs_x() {
        let ins = get_inst(0xBE);
        assert_eq!(ins.mnemonic, "LDX");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_bf() {
        let ins = get_inst(0xBF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpy_imm() {
        let ins = get_inst(0xC0);
        assert_eq!(ins.mnemonic, "CPY");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn cmp_ind_x() {
        let ins = get_inst(0xC1);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_c2() {
        let ins = get_inst(0xC2);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_c3() {
        let ins = get_inst(0xC3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpy_zp() {
        let ins = get_inst(0xC4);
        assert_eq!(ins.mnemonic, "CPY");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn cmp_zp() {
        let ins = get_inst(0xC5);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn dec_zp() {
        let ins = get_inst(0xC6);
        assert_eq!(ins.mnemonic, "DEC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_c7() {
        let ins = get_inst(0xC7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn iny_imp() {
        let ins = get_inst(0xC8);
        assert_eq!(ins.mnemonic, "INY");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn cmp_imm() {
        let ins = get_inst(0xC9);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn dex_imp() {
        let ins = get_inst(0xCA);
        assert_eq!(ins.mnemonic, "DEX");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_cb() {
        let ins = get_inst(0xCB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpy_abs() {
        let ins = get_inst(0xCC);
        assert_eq!(ins.mnemonic, "CPY");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn cmp_abs() {
        let ins = get_inst(0xCD);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn dec_abs() {
        let ins = get_inst(0xCE);
        assert_eq!(ins.mnemonic, "DEC");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_cf() {
        let ins = get_inst(0xCF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn bne_rel() {
        let ins = get_inst(0xD0);
        assert_eq!(ins.mnemonic, "BNE");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn cmp_ind_y() {
        let ins = get_inst(0xD1);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_d2() {
        let ins = get_inst(0xD2);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_d3() {
        let ins = get_inst(0xD3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_d4() {
        let ins = get_inst(0xD4);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cmp_zpx() {
        let ins = get_inst(0xD5);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn dec_zpx() {
        let ins = get_inst(0xD6);
        assert_eq!(ins.mnemonic, "DEC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_d7() {
        let ins = get_inst(0xD7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cld_imp() {
        let ins = get_inst(0xD8);
        assert_eq!(ins.mnemonic, "CLD");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn cmp_abs_y() {
        let ins = get_inst(0xD9);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_da() {
        let ins = get_inst(0xDA);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_db() {
        let ins = get_inst(0xDB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_dc() {
        let ins = get_inst(0xDC);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cmp_abs_x() {
        let ins = get_inst(0xDD);
        assert_eq!(ins.mnemonic, "CMP");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn dec_abs_x() {
        let ins = get_inst(0xDE);
        assert_eq!(ins.mnemonic, "DEC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn ill_df() {
        let ins = get_inst(0xDF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpx_imm() {
        let ins = get_inst(0xE0);
        assert_eq!(ins.mnemonic, "CPX");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn sbc_ind_x() {
        let ins = get_inst(0xE1);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_e2() {
        let ins = get_inst(0xE2);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_e3() {
        let ins = get_inst(0xE3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpx_zp() {
        let ins = get_inst(0xE4);
        assert_eq!(ins.mnemonic, "CPX");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn sbc_zp() {
        let ins = get_inst(0xE5);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 3);
    }

    #[test]
    pub fn inc_zp() {
        let ins = get_inst(0xE6);
        assert_eq!(ins.mnemonic, "INC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPage);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_e7() {
        let ins = get_inst(0xE7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn inx_imp() {
        let ins = get_inst(0xE8);
        assert_eq!(ins.mnemonic, "INX");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn sbc_imm() {
        let ins = get_inst(0xE9);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::Immediate);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn nop_imp() {
        let ins = get_inst(0xEA);
        assert_eq!(ins.mnemonic, "NOP");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn ill_eb() {
        let ins = get_inst(0xEB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn cpx_abs() {
        let ins = get_inst(0xEC);
        assert_eq!(ins.mnemonic, "CPX");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn sbc_abs() {
        let ins = get_inst(0xED);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn inc_abs() {
        let ins = get_inst(0xEE);
        assert_eq!(ins.mnemonic, "INC");
        assert_eq!(ins.addressing_mode, AddressMode::Absolute);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_ef() {
        let ins = get_inst(0xEF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn beq_rel() {
        let ins = get_inst(0xF0);
        assert_eq!(ins.mnemonic, "BEQ");
        assert_eq!(ins.addressing_mode, AddressMode::Relative);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn sbc_ind_y() {
        let ins = get_inst(0xF1);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::IndirectZeroY);
        assert_eq!(ins.cycle_cost, 5);
    }

    #[test]
    pub fn ill_f2() {
        let ins = get_inst(0xF2);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_f3() {
        let ins = get_inst(0xF3);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_f4() {
        let ins = get_inst(0xF4);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sbc_zpx() {
        let ins = get_inst(0xf5);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn inc_zpx() {
        let ins = get_inst(0xf6);
        assert_eq!(ins.mnemonic, "INC");
        assert_eq!(ins.addressing_mode, AddressMode::ZeroPageX);
        assert_eq!(ins.cycle_cost, 6);
    }

    #[test]
    pub fn ill_f7() {
        let ins = get_inst(0xF7);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sed_imp() {
        let ins = get_inst(0xF8);
        assert_eq!(ins.mnemonic, "SED");
        assert_eq!(ins.addressing_mode, AddressMode::Implied);
        assert_eq!(ins.cycle_cost, 2);
    }

    #[test]
    pub fn sbc_abs_y() {
        let ins = get_inst(0xF9);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn ill_fa() {
        let ins = get_inst(0xFA);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_fb() {
        let ins = get_inst(0xFB);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn ill_fc() {
        let ins = get_inst(0xFC);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }

    #[test]
    pub fn sbc_abs_x() {
        let ins = get_inst(0xFD);
        assert_eq!(ins.mnemonic, "SBC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 4);
    }

    #[test]
    pub fn inc_abs_x() {
        let ins = get_inst(0xFE);
        assert_eq!(ins.mnemonic, "INC");
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteX);
        assert_eq!(ins.cycle_cost, 7);
    }

    #[test]
    pub fn ill_ff() {
        let ins = get_inst(0xFF);
        assert_eq!(ins.mnemonic, "???");
        assert_eq!(ins.addressing_mode, AddressMode::None);
        assert_eq!(ins.cycle_cost, 0);
    }
}
