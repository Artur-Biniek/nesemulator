use super::bus::Memory;
use super::opcodes::AddressMode;
use super::opcodes::*;

struct Address(u16, bool);

const FLAG_N: u8 = 1 << 7;

const FLAG_Z: u8 = 1 << 1;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    status: u8,
    s: u8,
}

impl Cpu {
    pub fn new() -> Self {
        return Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            status: 0,
            s: 0xFF,
        };
    }

    pub fn step<T>(&mut self, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let ins = bus.read8(self.pc);
        self.pc += 1;

        let op = get_inst(ins);

        match ins {
            LDA_IMM | LDA_ZP | LDA_ZPX | LDA_IZX | LDA_IZY | LDA_ABS | LDA_ABX | LDA_ABY => {
                self.lda(op, bus)
            }
            STA_ZP | STA_ZPX | STA_IZX | STA_IZY | STA_ABS | STA_ABX | STA_ABY => self.sta(op, bus),
            LDX_IMM | LDX_ZP | LDX_ZPY | LDX_ABS | LDX_ABY => self.ldx(op, bus),
            STX_ZP | STX_ZPY | STX_ABS => self.stx(op, bus),
            LDY_IMM | LDY_ZP | LDY_ZPX | LDY_ABS | LDY_ABX => self.ldy(op, bus),
            STY_ZP | STY_ZPX | STY_ABS => self.sty(op, bus),
            TAX_IMP => self.tax(op),
            TXA_IMP => self.txa(op),
            TAY_IMP => self.tay(op),
            TYA_IMP => self.tya(op),
            TSX_IMP => self.tsx(op),
            TXS_IMP => self.txs(op),
            PHA_IMP => self.pha(op, bus),
            PLA_IMP => self.pla(op, bus),
            PHP_IMP => self.php(op, bus),
            PLP_IMP => self.plp(op, bus),
            // INX => self.inx(bus),
            // DEX => self.dex(bus),
            // // Panic
            o => panic!("unknown opcode: {:X}", o),
        }
    }

    fn get_address<T>(&mut self, mode: &AddressMode, bus: &T) -> Address
    where
        T: Memory,
    {
        match mode {
            AddressMode::Immediate => {
                let addr = self.pc;
                self.pc += 1;
                Address(addr, false)
            }
            AddressMode::ZeroPage => {
                let addr = bus.read8(self.pc) as u16;
                self.pc += 1;
                Address(addr, false)
            }
            AddressMode::ZeroPageX => {
                let addr = bus.read8(self.pc).wrapping_add(self.x) as u16;
                self.pc += 1;
                Address(addr, false)
            }
            AddressMode::ZeroPageY => {
                let addr = bus.read8(self.pc).wrapping_add(self.y) as u16;
                self.pc += 1;
                Address(addr, false)
            }
            AddressMode::Absolute => {
                let addr = bus.read16(self.pc);
                self.pc += 2;
                Address(addr, false)
            }
            AddressMode::AbsoluteX => {
                let base = bus.read16(self.pc);
                self.pc += 2;
                let addr = base.wrapping_add(self.x as u16);

                Address(addr, base & 0xFF00 != addr & 0xFF00)
            }
            AddressMode::AbsoluteY => {
                let base = bus.read16(self.pc);
                self.pc += 2;
                let addr = base.wrapping_add(self.y as u16);

                Address(addr, base & 0xFF00 != addr & 0xFF00)
            }
            AddressMode::IndirectZeroX => {
                let base = bus.read8(self.pc);
                self.pc += 1;
                let off = base.wrapping_add(self.x);
                let lo = bus.read8(off as u16) as u16;
                let hi = bus.read8(off.wrapping_add(1) as u16) as u16;

                Address(hi << 8 | lo, false)
            }
            AddressMode::IndirectZeroY => {
                let base = bus.read8(self.pc);
                self.pc += 1;

                let lo = bus.read8(base as u16) as u16;
                let hi = bus.read8(base.wrapping_add(1) as u16) as u16;

                let target = hi << 8 | lo;
                let fin = target.wrapping_add(self.y as u16);

                Address(fin, target & 0xFF00 != fin & 0xFF00)
            }
            _ => panic!("Unsupported addresing mode"),
        }
    }

    fn set_flag(&mut self, flag: u8, value: bool) {
        if value {
            self.status |= flag;
        } else {
            self.status &= !flag;
        }
    }

    fn get_flag(&self, flag: u8) -> bool {
        self.status & flag != 0
    }

    fn set_zero_and_negative_flags(&mut self, value: u8) {
        self.set_flag(FLAG_N, (value as i8) < 0);
        self.set_flag(FLAG_Z, value == 0);
    }

    fn lda<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.a = bus.read8(addr);
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn sta<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        bus.write8(addr, self.a);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn ldx<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.x = bus.read8(addr);
        self.set_zero_and_negative_flags(self.x);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn stx<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        bus.write8(addr, self.x);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn ldy<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.y = bus.read8(addr);
        self.set_zero_and_negative_flags(self.y);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn sty<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        bus.write8(addr, self.y);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn tax(&mut self, inst: &Instruction) -> u8 {
        self.x = self.a;
        self.set_zero_and_negative_flags(self.x);
        inst.cycle_cost
    }
    fn txa(&mut self, inst: &Instruction) -> u8 {
        self.a = self.x;
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost
    }
    fn tay(&mut self, inst: &Instruction) -> u8 {
        self.y = self.a;
        self.set_zero_and_negative_flags(self.y);
        inst.cycle_cost
    }
    fn tya(&mut self, inst: &Instruction) -> u8 {
        self.a = self.y;
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost
    }
    fn tsx(&mut self, inst: &Instruction) -> u8 {
        self.x = self.s;
        self.set_zero_and_negative_flags(self.x);
        inst.cycle_cost
    }
    fn txs(&mut self, inst: &Instruction) -> u8 {
        self.s = self.x;
        inst.cycle_cost
    }
    fn pha<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        bus.write8(0x0100 | (self.s as u16), self.a);
        self.s = self.s.wrapping_sub(1);
        inst.cycle_cost
    }
    fn pla<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        self.s = self.s.wrapping_add(1);
        self.a = bus.read8(0x0100 | (self.s as u16));
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost
    }
    fn php<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        bus.write8(0x0100 | (self.s as u16), self.status);
        self.s = self.s.wrapping_sub(1);
        inst.cycle_cost
    }
    fn plp<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        self.s = self.s.wrapping_add(1);
        self.status = bus.read8(0x0100 | (self.s as u16));
        inst.cycle_cost
    }
    // fn dex(&mut self, bus: &mut Bus) {
    //     self.x = self.x.wrapping_sub(1);
    //     self.set_zero_and_negative_flags(self.x);
    //     self.set_register_instruction_cycles(bus);
    // }
    // fn inx(&mut self, bus: &mut Bus) {
    //     self.x = self.x.wrapping_add(1);
    //     self.set_zero_and_negative_flags(self.x);
    //     self.set_register_instruction_cycles(bus);
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct Bus {
        data: HashMap<u16, u8>,
    }

    impl Bus {
        pub fn new(prg_rom: Vec<u8>) -> Self {
            let mut r = Self {
                data: HashMap::new(),
            };
            for (i, b) in prg_rom.iter().enumerate() {
                r.data.insert(i as u16, *b);
            }
            r
        }
    }

    impl Memory for Bus {
        fn write8(&mut self, addr: u16, v: u8) {
            self.data.insert(addr, v);
        }
        fn read8(&self, addr: u16) -> u8 {
            if let Some(v) = self.data.get(&addr) {
                *v
            } else {
                panic!("Address {:X} empty", addr);
            }
        }
        fn read16(&self, addr: u16) -> u16 {
            let lo = self.read8(addr) as u16;
            let hi = self.read8(addr + 1) as u16;

            (hi << 8) | lo
        }
    }

    #[test]
    fn test_state_after_new() {
        let cpu = Cpu::new();
        assert_eq!(cpu.a, 0);
        assert!(!cpu.get_flag(FLAG_Z));
        assert!(!cpu.get_flag(FLAG_N));
    }

    #[test]
    fn test_lda_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDA_IMM, val]);
            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDA_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDA_ZPX, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_lda_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDA_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_lda_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![LDA_ABX, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }
    #[test]
    fn test_lda_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![LDA_ABY, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_lda_indirect_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![LDA_IZX, 0x04]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base, val);

            let mut cpu = Cpu::new();
            cpu.x = 0x10;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 6);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
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
            let mut bus = Bus::new(vec![LDA_IZY, 0x14]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base.wrapping_add(y as u16), val);

            let mut cpu = Cpu::new();
            cpu.y = y;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, if page_crossed { 6 } else { 5 });

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_sta_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STA_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_sta_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STA_ZPX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val.wrapping_add(0xf0) as u16), val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sta_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STA_ABS, 0x0f, 0x15]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(0x150f), val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sta_absolute_x() {
        for val in 0..=255 {
            let lo = 0x15;
            let hi = 0x0f;
            let base = (hi as u16) << 8 | lo as u16;
            let target = base.wrapping_add(val as u16);

            let mut bus = Bus::new(vec![STA_ABX, lo, hi]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(target), val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(
                cycles,
                if target & 0xff00 == base & 0xff00 {
                    5
                } else {
                    6
                }
            );
        }
    }

    #[test]
    fn test_sta_absolute_y() {
        for val in 0..=255 {
            let lo = 0x15;
            let hi = 0x0f;
            let base = (hi as u16) << 8 | lo as u16;
            let target = base.wrapping_add(val as u16);

            let mut bus = Bus::new(vec![STA_ABY, lo, hi]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(target), val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(
                cycles,
                if target & 0xff00 == base & 0xff00 {
                    5
                } else {
                    6
                }
            );
        }
    }

    #[test]
    fn test_ldx_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDX_IMM, val]);
            let mut cpu = Cpu::new();

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDX_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDX_ZPY, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.y = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDX_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![LDX_ABY, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_stx_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STX_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_stx_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STX_ZPY, 0x05]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, bus.read8(cpu.y.wrapping_add(0x05) as u16));
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_stx_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STX_ABS, 0x05, 0xf0]);
            let mut cpu = Cpu::new();

            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, bus.read8(0xf005));
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sty_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STY_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_sty_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STY_ZPX, 0x05]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.y = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, bus.read8(cpu.x.wrapping_add(0x05) as u16));
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sty_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![STY_ABS, 0x05, 0xf0]);
            let mut cpu = Cpu::new();

            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, bus.read8(0xf005));
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_ldy_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDY_IMM, val]);
            let mut cpu = Cpu::new();

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDY_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDY_ZPX, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![LDY_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(vec![LDY_ABX, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
        }
    }

    #[test]
    fn test_tax() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TAX_IMP]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, cpu.x);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_txa() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TXA_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, cpu.a);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_tay() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TAY_IMP]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, cpu.y);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_tya() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TYA_IMP]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, cpu.a);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_tsx() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TSX_IMP]);
            let mut cpu = Cpu::new();
            cpu.s = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, cpu.x);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_txs() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![TXS_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, cpu.s);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_pha() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PHA_IMP]);
            bus.write8(0x01ff, 0);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xfe);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_pla() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PLA_IMP]);
            bus.write8(0x01ff, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xfe;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cpu.get_flag(FLAG_N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(FLAG_Z), val == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_php() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PHP_IMP]);
            bus.write8(0x01ff, 0);
            let mut cpu = Cpu::new();
            cpu.status = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xfe);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_plp() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PLP_IMP]);
            bus.write8(0x01ff, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xfe;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.status, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_stack_wrap_push() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PHA_IMP]);
            bus.write8(0x0100, val);
            let mut cpu = Cpu::new();
            cpu.a = val;
            cpu.s = 0x00;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(0x0100), val);
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc, 1);
        }
    }

    #[test]
    fn test_stack_wrap_pull() {
        for val in 0..=255 {
            let mut bus = Bus::new(vec![PLA_IMP]);
            bus.write8(0x0100, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.s, 0x00);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc, 1);
        }
    }
    // #[test]
    // fn test_dex() {
    //     for val in 0..=255 {
    //         let mut bus = Bus::new(vec![DEX]);
    //         let mut cpu = Cpu::new();
    //
    //         cpu.x = val;
    //         let cycles = cpu.step(&mut bus);

    //         assert_eq!(cpu.x, val.wrapping_sub(1));
    //         assert_eq!(cpu.get_flag(FLAG_N), (cpu.x as i8) < 0);
    //         assert_eq!(cpu.get_flag(FLAG_Z), cpu.x == 0);
    //         assert_eq!(cycles, 2);
    //         assert_eq!(cpu.pc,  1);
    //     }
    // }
    // #[test]
    // fn test_inx() {
    //     for val in 0..=255 {
    //         let mut bus = Bus::new(vec![INX]);
    //         let mut cpu = Cpu::new();
    //
    //         cpu.x = val;
    //         let cycles = cpu.step(&mut bus);

    //         assert_eq!(cpu.x, val.wrapping_add(1));
    //         assert_eq!(cpu.get_flag(FLAG_N), (cpu.x as i8) < 0);
    //         assert_eq!(cpu.get_flag(FLAG_Z), cpu.x == 0);
    //         assert_eq!(cycles, 2);
    //         assert_eq!(cpu.pc,  1);
    //     }
}
