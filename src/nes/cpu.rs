use super::bus;
use super::bus::Bus;
use super::opcodes as op;
use super::opcodes::AddressMode;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    zero_flag: bool,
    negative_flag: bool,
}

impl Cpu {
    pub fn new() -> Self {
        return Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            zero_flag: false,
            negative_flag: false,
        };
    }

    pub fn step(&mut self, bus: &mut Bus) {
        let ins = bus.read8(self.pc);
        self.pc += 1;

        match ins {
            // LDA
            op::LDA_IMM => self.lda(AddressMode::Immediate, bus),
            op::LDA_ZP => self.lda(AddressMode::ZeroPage, bus),
            op::LDA_ZP_X => self.lda(AddressMode::ZeroPageX, bus),
            op::LDA_ABS => self.lda(AddressMode::Absolute, bus),
            op::LDA_ABS_X => self.lda(AddressMode::AbsoluteX, bus),
            op::LDA_ABS_Y => self.lda(AddressMode::AbsoluteY, bus),
            op::LDA_IND_X => self.lda(AddressMode::IndirectX, bus),
            op::LDA_IND_Y => self.lda(AddressMode::IndirectY, bus),
            // LDX
            op::LDX_IMM => self.ldx(AddressMode::Immediate, bus),
            op::LDX_ZP => self.ldx(AddressMode::ZeroPage, bus),
            op::LDX_ZP_Y => self.ldx(AddressMode::ZeroPageY, bus),
            op::LDX_ABS => self.ldx(AddressMode::Absolute, bus),
            op::LDX_ABS_Y => self.ldx(AddressMode::AbsoluteY, bus),
            // LDY
            op::LDY_IMM => self.ldy(AddressMode::Immediate, bus),
            op::LDY_ZP => self.ldy(AddressMode::ZeroPage, bus),
            op::LDY_ZP_X => self.ldy(AddressMode::ZeroPageX, bus),
            op::LDY_ABS => self.ldy(AddressMode::Absolute, bus),
            op::LDY_ABS_X => self.ldy(AddressMode::AbsoluteX, bus),
            // Panic
            o => panic!("unknown opcode: {:X}", o),
        }
    }
    fn get_address(&self, mode: &AddressMode, bus: &Bus) -> (u16, bool) {
        match mode {
            AddressMode::Immediate => (self.pc, false),
            AddressMode::ZeroPage => (bus.read8(self.pc) as u16, false),
            AddressMode::ZeroPageX => (bus.read8(self.pc).wrapping_add(self.x) as u16, false),
            AddressMode::ZeroPageY => (bus.read8(self.pc).wrapping_add(self.y) as u16, false),
            AddressMode::Absolute => (bus.read16(self.pc), false),
            AddressMode::AbsoluteX => {
                let base = bus.read16(self.pc);
                let target = base.wrapping_add(self.x as u16);
                (target, base & 0xFF00 != target & 0xFF00)
            }
            AddressMode::AbsoluteY => {
                let base = bus.read16(self.pc);
                let target = base.wrapping_add(self.y as u16);
                (target, base & 0xFF00 != target & 0xFF00)
            }
            AddressMode::IndirectX => {
                let base = bus.read8(self.pc);
                let off = base.wrapping_add(self.x);

                let lo = bus.read8(off as u16) as u16;
                let hi = bus.read8(off.wrapping_add(1) as u16) as u16;

                (hi << 8 | lo, false)
            }
            AddressMode::IndirectY => {
                let base = bus.read8(self.pc);
                let lo = bus.read8(base as u16) as u16;
                let hi = bus.read8(base.wrapping_add(1) as u16) as u16;

                let target = hi << 8 | lo;
                let fin = target.wrapping_add(self.y as u16);

                (fin, target & 0xFF00 != fin & 0xFF00)
            }
        }
    }

    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.zero_flag = value == 0;
        self.negative_flag = (value as i8) < 0;
    }

    fn lda(&mut self, mode: AddressMode, bus: &mut Bus) {
        let (addr, page_crossed) = self.get_address(&mode, bus);
        self.a = bus.read8(addr);
        self.set_zero_and_negative_flags(self.a);

        match mode {
            AddressMode::Immediate => {
                bus.tick(2);
                self.pc += 1;
            }
            AddressMode::ZeroPage => {
                bus.tick(3);
                self.pc += 1;
            }
            AddressMode::ZeroPageX => {
                bus.tick(4);
                self.pc += 1;
            }
            AddressMode::Absolute => {
                bus.tick(4);
                self.pc += 2;
            }
            AddressMode::AbsoluteX | AddressMode::AbsoluteY => {
                bus.tick(if page_crossed { 5 } else { 4 });
                self.pc += 2;
            }
            AddressMode::IndirectX => {
                bus.tick(6);
                self.pc += 1;
            }
            AddressMode::IndirectY => {
                bus.tick(if page_crossed { 6 } else { 5 });
                self.pc += 1;
            }
            _ => panic!("Unsupported addressing mode"),
        }
    }
    fn ldx(&mut self, mode: AddressMode, bus: &mut Bus) {
        let (addr, page_crossed) = self.get_address(&mode, bus);
        self.x = bus.read8(addr);
        self.set_zero_and_negative_flags(self.x);

        match mode {
            AddressMode::Immediate => {
                bus.tick(2);
                self.pc += 1;
            }
            AddressMode::ZeroPage => {
                bus.tick(3);
                self.pc += 1;
            }
            AddressMode::ZeroPageY => {
                bus.tick(4);
                self.pc += 1;
            }
            AddressMode::Absolute => {
                bus.tick(4);
                self.pc += 2;
            }
            AddressMode::AbsoluteY => {
                bus.tick(if page_crossed { 5 } else { 4 });
                self.pc += 2;
            }
            _ => panic!("Unsupported address mode"),
        }
    }
    fn ldy(&mut self, mode: AddressMode, bus: &mut Bus) {
        let (addr, page_crossed) = self.get_address(&mode, bus);
        self.y = bus.read8(addr);
        self.set_zero_and_negative_flags(self.y);

        match mode {
            AddressMode::Immediate => {
                bus.tick(2);
                self.pc += 1;
            }
            AddressMode::ZeroPage => {
                bus.tick(3);
                self.pc += 1;
            }
            AddressMode::ZeroPageX => {
                bus.tick(4);
                self.pc += 1;
            }
            AddressMode::Absolute => {
                bus.tick(4);
                self.pc += 2;
            }
            AddressMode::AbsoluteX => {
                bus.tick(if page_crossed { 5 } else { 4 });
                self.pc += 2;
            }
            _ => panic!("Unsupported address mode"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::opcodes as op;

    #[test]
    fn test_state_after_new() {
        let cpu = Cpu::new();
        assert_eq!(cpu.a, 0);
        assert!(!cpu.zero_flag);
        assert!(!cpu.negative_flag);
    }

    #[test]
    fn test_lda_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDA_IMM, val]);
            let mut cpu = Cpu::new();

            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 2);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDA_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 3);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDA_ZP_X, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.x = 0xFF;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDA_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDA_ABS_X, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(bus.get_cycles(), 5);
            } else {
                assert_eq!(bus.get_cycles(), 4);
            }

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }
    #[test]
    fn test_lda_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDA_ABS_Y, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(bus.get_cycles(), 5);
            } else {
                assert_eq!(bus.get_cycles(), 4);
            }

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_indirect_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDA_IND_X, 0x04]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.x = 0x10;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 6);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_lda_indirect_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let page_crossed = val % 2 == 0;
            let y: u8 = if page_crossed { 0x51 } else { 0x02 };
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDA_IND_Y, 0x14]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base.wrapping_add(y as u16), val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.y = y;
            cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), if page_crossed { 6 } else { 5 });

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldx_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDX_IMM, val]);
            let mut cpu = Cpu::new();

            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 2);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDX_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 3);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDX_ZP_Y, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.y = 0xFF;
            cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDX_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDX_ABS_Y, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(bus.get_cycles(), 5);
            } else {
                assert_eq!(bus.get_cycles(), 4);
            }

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldy_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDY_IMM, val]);
            let mut cpu = Cpu::new();

            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 2);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDY_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 3);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDY_ZP_X, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.x = 0xFF;
            cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 2);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![op::LDY_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);
            assert_eq!(bus.get_cycles(), 4);

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![op::LDY_ABS_X, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.pc = bus::PRG_ROM_START;
            cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, bus::PRG_ROM_START + 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(bus.get_cycles(), 5);
            } else {
                assert_eq!(bus.get_cycles(), 4);
            }

            assert_eq!(cpu.negative_flag, (val as i8) < 0);
            assert_eq!(cpu.zero_flag, val == 0);
        }
    }
}
