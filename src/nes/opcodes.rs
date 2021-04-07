use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AddressMode {
    None,
    Implied,
    Immediate,
    ZeroPage,
    ZeroPageX,
    IndirectZeroX,
    IndirectZeroY,
    ZeroPageY,
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
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY                   );
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
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY                   );
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
        assert_eq!(ins.addressing_mode, AddressMode::AbsoluteY                   );
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
}
