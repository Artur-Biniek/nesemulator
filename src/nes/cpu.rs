use super::bus::Memory;
use super::opcodes::AddressMode;
use super::opcodes::*;

struct Address(u16, bool);

mod flags {
    pub const N: u8 = 1 << 7;
    pub const V: u8 = 1 << 6;
    // 1 << 5 unused
    pub const B: u8 = 1 << 4;
    pub const D: u8 = 1 << 3;
    pub const I: u8 = 1 << 2;
    pub const Z: u8 = 1 << 1;
    pub const C: u8 = 1 << 0;
}

mod vectors {
    pub const IRQ: u16 = 0xFFFE;
}

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
            pc: 0x8000,
            status: 0,
            s: 0,
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
            ORA_IMM | ORA_ZP | ORA_ZPX | ORA_IZX | ORA_IZY | ORA_ABS | ORA_ABX | ORA_ABY => {
                self.ora(op, bus)
            }
            AND_IMM | AND_ZP | AND_ZPX | AND_IZX | AND_IZY | AND_ABS | AND_ABX | AND_ABY => {
                self.and(op, bus)
            }
            EOR_IMM | EOR_ZP | EOR_ZPX | EOR_IZX | EOR_IZY | EOR_ABS | EOR_ABX | EOR_ABY => {
                self.eor(op, bus)
            }
            ADC_IMM | ADC_ZP | ADC_ZPX | ADC_IZX | ADC_IZY | ADC_ABS | ADC_ABX | ADC_ABY => {
                self.adc(op, bus)
            }
            SBC_IMM | SBC_ZP | SBC_ZPX | SBC_IZX | SBC_IZY | SBC_ABS | SBC_ABX | SBC_ABY => {
                self.sbc(op, bus)
            }
            CMP_IMM | CMP_ZP | CMP_ZPX | CMP_IZX | CMP_IZY | CMP_ABS | CMP_ABX | CMP_ABY => {
                self.cmp(op, bus)
            }
            CPX_IMM | CPX_ZP | CPX_ABS => self.cpx(op, bus),
            CPY_IMM | CPY_ZP | CPY_ABS => self.cpy(op, bus),
            DEC_ZP | DEC_ZPX | DEC_ABS | DEC_ABX => self.dec(op, bus),
            INC_ZP | INC_ZPX | INC_ABS | INC_ABX => self.inc(op, bus),
            INX_IMP => self.inx(op),
            DEX_IMP => self.dex(op),
            INY_IMP => self.iny(op),
            DEY_IMP => self.dey(op),
            ASL_IMP | ASL_ZP | ASL_ZPX | ASL_ABS | ASL_ABX => self.asl(op, bus),
            ROL_IMP | ROL_ZP | ROL_ZPX | ROL_ABS | ROL_ABX => self.rol(op, bus),
            LSR_IMP | LSR_ZP | LSR_ZPX | LSR_ABS | LSR_ABX => self.lsr(op, bus),
            ROR_IMP | ROR_ZP | ROR_ZPX | ROR_ABS | ROR_ABX => self.ror(op, bus),
            BPL_REL => self.branch(op, bus, flags::N, false),
            BMI_REL => self.branch(op, bus, flags::N, true),
            BVC_REL => self.branch(op, bus, flags::V, false),
            BVS_REL => self.branch(op, bus, flags::V, true),
            BCC_REL => self.branch(op, bus, flags::C, false),
            BCS_REL => self.branch(op, bus, flags::C, true),
            BNE_REL => self.branch(op, bus, flags::Z, false),
            BEQ_REL => self.branch(op, bus, flags::Z, true),
            BRK_IMP => self.brk(op, bus),
            RTI_IMP => self.rti(op, bus),
            JSR_ABS => self.jsr(op, bus),
            RTS_IMP => self.rts(op, bus),
            JMP_ABS => self.jmp(op, bus),
            JMP_IND => self.jmp(op, bus),
            // Panic
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
            AddressMode::Relative => {
                let addr = bus.read8(self.pc) as i8;
                self.pc += 1;
 
                let target = self.pc.wrapping_add(addr as u16);

                Address(target, target & 0xFF00 != self.pc & 0xFF00)
            }
            AddressMode::Indirect => {
                let lo = bus.read8(self.pc);
                self.pc += 1;
                let hi = bus.read8(self.pc);
                self.pc += 1;

                let ind = u16::from_le_bytes([lo, hi]);
                let nlo = bus.read8(ind);
                
                let bugged_ind = u16::from_le_bytes([lo.wrapping_add(1), hi]);
                let nhi = bus.read8(bugged_ind);
               
                let target = u16::from_le_bytes([nlo, nhi]);

                Address(target, false)
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
        self.set_flag(flags::N, (value as i8) < 0);
        self.set_flag(flags::Z, value == 0);
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
    fn ora<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.a |= bus.read8(addr);
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn and<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.a &= bus.read8(addr);
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn eor<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        self.a ^= bus.read8(addr);
        self.set_zero_and_negative_flags(self.a);
        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn adc<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let arg = bus.read8(addr);
        let carry = if self.get_flag(flags::C) { 1 } else { 0 };
        let arg_with_c = arg.wrapping_add(carry);
        let a = self.a;

        self.a = a.wrapping_add(arg_with_c);
        self.set_zero_and_negative_flags(self.a);

        let c6 = ((0x7F & a as u16) + (0x7F & arg as u16) + (carry as u16)) & 0x080 != 0;
        let c7 = ((0xFF & a as u16) + (0xFF & arg as u16) + (carry as u16)) & 0x100 != 0;

        self.set_flag(flags::C, (a as u16 + arg as u16 + carry as u16) > 0xff);
        self.set_flag(flags::V, c6 ^ c7);

        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn sbc<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let arg = !bus.read8(addr);
        let carry = if self.get_flag(flags::C) { 1 } else { 0 };
        let arg_with_c = arg.wrapping_add(carry);
        let a = self.a;

        self.a = a.wrapping_add(arg_with_c);
        self.set_zero_and_negative_flags(self.a);

        let c6 = ((0x7F & a as u16) + (0x7F & arg as u16) + (carry as u16)) & 0x080 != 0;
        let c7 = ((0xFF & a as u16) + (0xFF & arg as u16) + (carry as u16)) & 0x100 != 0;

        self.set_flag(flags::C, (a as u16 + arg as u16 + carry as u16) > 0xff);
        self.set_flag(flags::V, c6 ^ c7);

        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn cmp<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let tmp = self.a.wrapping_sub(m);

        self.set_zero_and_negative_flags(tmp);
        self.set_flag(flags::C, self.a >= m);

        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn cpx<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let tmp = self.x.wrapping_sub(m);

        self.set_zero_and_negative_flags(tmp);
        self.set_flag(flags::C, self.x >= m);

        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn cpy<T>(&mut self, inst: &Instruction, bus: &T) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let tmp = self.y.wrapping_sub(m);

        self.set_zero_and_negative_flags(tmp);
        self.set_flag(flags::C, self.y >= m);

        inst.cycle_cost + if page_crossed { 1 } else { 0 }
    }
    fn dec<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = m.wrapping_sub(1);
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);

        inst.cycle_cost
    }
    fn inc<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = m.wrapping_add(1);
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);

        inst.cycle_cost
    }
    fn dex(&mut self, inst: &Instruction) -> u8 {
        self.x = self.x.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.x);
        inst.cycle_cost
    }
    fn inx(&mut self, inst: &Instruction) -> u8 {
        self.x = self.x.wrapping_add(1);
        self.set_zero_and_negative_flags(self.x);
        inst.cycle_cost
    }
    fn dey(&mut self, inst: &Instruction) -> u8 {
        self.y = self.y.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.y);
        inst.cycle_cost
    }
    fn iny(&mut self, inst: &Instruction) -> u8 {
        self.y = self.y.wrapping_add(1);
        self.set_zero_and_negative_flags(self.y);
        inst.cycle_cost
    }
    fn asl<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = m << 1;
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);
        self.set_flag(flags::C, m & 0x80 != 0);

        inst.cycle_cost
    }
    fn rol<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = (m << 1) + if self.get_flag(flags::C) { 1 } else { 0 };
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);
        self.set_flag(flags::C, m & 0x80 != 0);

        inst.cycle_cost
    }
    fn lsr<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = m >> 1;
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);
        self.set_flag(flags::C, m & 0x01 != 0);

        inst.cycle_cost
    }
    fn ror<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(addr, _) = self.get_address(&inst.addressing_mode, bus);
        let m = bus.read8(addr);
        let r = (m >> 1) + 128 * (if self.get_flag(flags::C) { 1 } else { 0 });
        bus.write8(addr, r);

        self.set_zero_and_negative_flags(r);
        self.set_flag(flags::C, m & 0x01 != 0);

        inst.cycle_cost
    }
    fn branch<T>(&mut self, inst: &Instruction, bus: &mut T, flag: u8, branch_when: bool) -> u8
    where
        T: Memory,
    {
        let Address(addr, page_crossed) = self.get_address(&inst.addressing_mode, bus);
        let branch_taken = self.get_flag(flag) == branch_when;

        if branch_taken {
            self.pc = addr;
        }

        inst.cycle_cost
            + if branch_taken {
                1 + if page_crossed { 1 } else { 0 }
            } else {
                0
            }
    }
    fn brk<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let pc_bytes = self.pc.to_le_bytes();

        bus.write8(0x0100 | self.s as u16, pc_bytes[1]);
        self.s = self.s.wrapping_sub(1);
        bus.write8(0x0100 | self.s as u16, pc_bytes[0]);
        self.s = self.s.wrapping_sub(1);
        bus.write8(0x0100 | self.s as u16, self.status);
        self.s = self.s.wrapping_sub(1);

        self.set_flag(flags::B, true);
        self.set_flag(flags::I, true);

        let irq_lo = bus.read8(vectors::IRQ);
        let irq_hi = bus.read8(vectors::IRQ.wrapping_add(1));

        self.pc = u16::from_le_bytes([irq_lo, irq_hi]);

        inst.cycle_cost
    }
    fn rti<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        self.s = self.s.wrapping_add(1);
        let status = bus.read8(0x0100 | self.s as u16);
        self.s = self.s.wrapping_add(1);
        let pc_lo = bus.read8(0x0100 | self.s as u16);
        self.s = self.s.wrapping_add(1);
        let pc_hi = bus.read8(0x0100 | self.s as u16);

        self.status = status;
        self.pc = u16::from_le_bytes([pc_lo, pc_hi]);

        inst.cycle_cost
    }
    fn jsr<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(pc_next, _) = self.get_address(&inst.addressing_mode, bus);

        let pc_min_one = self.pc.wrapping_sub(1);
        let pc_bytes = pc_min_one.to_le_bytes();

        bus.write8(0x0100 | self.s as u16, pc_bytes[1]);
        self.s = self.s.wrapping_sub(1);
        bus.write8(0x0100 | self.s as u16, pc_bytes[0]);
        self.s = self.s.wrapping_sub(1);

        self.pc = pc_next;

        inst.cycle_cost
    }
    fn rts<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        self.s = self.s.wrapping_add(1);
        let pc_lo = bus.read8(0x0100 | self.s as u16);
        self.s = self.s.wrapping_add(1);
        let pc_hi = bus.read8(0x0100 | self.s as u16);

        let pc_less_one = u16::from_le_bytes([pc_lo, pc_hi]);
        let next_pc = pc_less_one.wrapping_add(1);

        self.pc = next_pc;

        inst.cycle_cost
    }
    fn jmp<T>(&mut self, inst: &Instruction, bus: &mut T) -> u8
    where
        T: Memory,
    {
        let Address(pc_next, _) = self.get_address(&inst.addressing_mode, bus);

        self.pc = pc_next;

        inst.cycle_cost
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct Bus {
        data: HashMap<u16, u8>,
    }

    impl Bus {
        pub fn new(org: u16, prg_rom: Vec<u8>) -> Self {
            let mut r = Self {
                data: HashMap::new(),
            };
            for (i, b) in prg_rom.iter().enumerate() {
                r.data.insert(org + i as u16, *b);
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
        assert!(!cpu.get_flag(flags::Z));
        assert!(!cpu.get_flag(flags::N));
    }

    #[test]
    fn test_lda_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDA_IMM, val]);
            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDA_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_lda_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDA_ZPX, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_lda_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDA_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_lda_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(0x8000, vec![LDA_ABX, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }
    #[test]
    fn test_lda_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(0x8000, vec![LDA_ABY, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_lda_indirect_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(0x8000, vec![LDA_IZX, 0x04]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base, val);

            let mut cpu = Cpu::new();
            cpu.x = 0x10;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 6);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
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
            let mut bus = Bus::new(0x8000, vec![LDA_IZY, 0x14]);

            bus.write8(0x14, base_lo);
            bus.write8(0x15, base_hi);
            bus.write8(base.wrapping_add(y as u16), val);

            let mut cpu = Cpu::new();
            cpu.y = y;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, if page_crossed { 6 } else { 5 });

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_sta_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STA_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_sta_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STA_ZPX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val.wrapping_add(0xf0) as u16), val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sta_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STA_ABS, 0x0f, 0x15]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(0x150f), val);
            assert_eq!(cpu.pc - 0x8000, 3);
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

            let mut bus = Bus::new(0x8000, vec![STA_ABX, lo, hi]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(target), val);
            assert_eq!(cpu.pc - 0x8000, 3);
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

            let mut bus = Bus::new(0x8000, vec![STA_ABY, lo, hi]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(target), val);
            assert_eq!(cpu.pc - 0x8000, 3);
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
            let mut bus = Bus::new(0x8000, vec![LDX_IMM, val]);
            let mut cpu = Cpu::new();

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDX_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDX_ZPY, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.y = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDX_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc - 0x8000, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldx_absolute_y() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(0x8000, vec![LDX_ABY, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val);
            assert_eq!(cpu.pc - 0x8000, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_stx_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STX_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_stx_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STX_ZPY, 0x05]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, bus.read8(cpu.y.wrapping_add(0x05) as u16));
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_stx_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STX_ABS, 0x05, 0xf0]);
            let mut cpu = Cpu::new();

            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, bus.read8(0xf005));
            assert_eq!(cpu.pc - 0x8000, 3);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sty_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STY_ZP, val]);

            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(val as u16), val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);
        }
    }

    #[test]
    fn test_sty_zero_page_y() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STY_ZPX, 0x05]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            cpu.y = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, bus.read8(cpu.x.wrapping_add(0x05) as u16));
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_sty_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![STY_ABS, 0x05, 0xf0]);
            let mut cpu = Cpu::new();

            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, bus.read8(0xf005));
            assert_eq!(cpu.pc - 0x8000, 3);
            assert_eq!(cycles, 4);
        }
    }

    #[test]
    fn test_ldy_immediate() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDY_IMM, val]);
            let mut cpu = Cpu::new();

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 2);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDY_ZP, 0x14]);
            bus.write8(0x14, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 3);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_zero_page_x() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDY_ZPX, 0x05]);
            bus.write8(0x04, val);

            let mut cpu = Cpu::new();
            cpu.x = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc - 0x8000, 2);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![LDY_ABS, 0x05, 0x02]);
            bus.write8(0x0205, val);

            let mut cpu = Cpu::new();
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc - 0x8000, 3);
            assert_eq!(cycles, 4);

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_ldy_absolute_x() {
        for val in 0..=255 {
            let base_lo = 0xF0;
            let base_hi = 0x02;
            let base = (base_hi as u16) << 8 | (base_lo as u16);
            let mut bus = Bus::new(0x8000, vec![LDY_ABX, base_lo, base_hi]);
            let target = base.wrapping_add(val as u16);
            bus.write8(target, val);

            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val);
            assert_eq!(cpu.pc - 0x8000, 3);

            let page_crossed = target & 0xFF00 != 0x0200;
            if page_crossed {
                assert_eq!(cycles, 5);
            } else {
                assert_eq!(cycles, 4);
            }

            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
        }
    }

    #[test]
    fn test_tax() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TAX_IMP]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, cpu.x);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_txa() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TXA_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, cpu.a);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_tay() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TAY_IMP]);
            let mut cpu = Cpu::new();
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, cpu.y);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_tya() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TYA_IMP]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, cpu.a);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_tsx() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TSX_IMP]);
            let mut cpu = Cpu::new();
            cpu.s = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, cpu.x);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_txs() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![TXS_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, cpu.s);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_pha() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PHA_IMP]);
            bus.write8(0x01ff, 0);
            let mut cpu = Cpu::new();
            cpu.s = 0xff;
            cpu.a = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xfe);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_pla() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PLA_IMP]);
            bus.write8(0x01ff, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xfe;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cpu.get_flag(flags::N), (val as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), val == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_php() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PHP_IMP]);
            bus.write8(0x01ff, 0);
            let mut cpu = Cpu::new();
            cpu.s = 0xff;
            cpu.status = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(val, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xfe);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_plp() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PLP_IMP]);
            bus.write8(0x01ff, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xfe;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.status, bus.read8(0x1ff));
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_stack_wrap_push() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PHA_IMP]);
            bus.write8(0x0100, val);
            let mut cpu = Cpu::new();
            cpu.a = val;
            cpu.s = 0x00;
            let cycles = cpu.step(&mut bus);

            assert_eq!(bus.read8(0x0100), val);
            assert_eq!(cpu.s, 0xff);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_stack_wrap_pull() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![PLA_IMP]);
            bus.write8(0x0100, val);
            let mut cpu = Cpu::new();
            cpu.s = 0xFF;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, val);
            assert_eq!(cpu.s, 0x00);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_dex() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![DEX_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val.wrapping_sub(1));
            assert_eq!(cpu.get_flag(flags::N), (cpu.x as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.x == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_inx() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![INX_IMP]);
            let mut cpu = Cpu::new();
            cpu.x = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.x, val.wrapping_add(1));
            assert_eq!(cpu.get_flag(flags::N), (cpu.x as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.x == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_ora_imm() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ORA_IMM, val]);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_ora_zp() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ORA_ZP, val]);
            bus.write8(val as u16, val);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_ora_zpx() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ORA_ZPX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            bus.write8(val.wrapping_add(cpu.x) as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_ora_abs() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ORA_ABS, val, 0xf0]);

            let mut cpu = Cpu::new();
            bus.write8(0xf000 + val as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_ora_abx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![ORA_ABX, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.x as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_ora_aby() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![ORA_ABY, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_ora_izx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![ORA_IZX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            bus.write8(base, !val);

            let target = val.wrapping_add(cpu.x) as u16;
            let target_p1 = val.wrapping_add(cpu.x + 1) as u16;
            bus.write8(target, lo);
            bus.write8(target_p1, hi);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_ora_izy() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![ORA_IZY, val]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;

            bus.write8(val as u16, lo);
            bus.write8(val.wrapping_add(1) as u16, hi);

            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 | !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 6 } else { 5 });
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_and_imm() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![AND_IMM, val]);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_and_zp() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![AND_ZP, val]);
            bus.write8(val as u16, val);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_and_zpx() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![AND_ZPX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            bus.write8(val.wrapping_add(cpu.x) as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_and_abs() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![AND_ABS, val, 0xf0]);

            let mut cpu = Cpu::new();
            bus.write8(0xf000 + val as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_and_abx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![AND_ABX, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.x as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_and_aby() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![AND_ABY, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_and_izx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![AND_IZX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            bus.write8(base, !val);

            let target = val.wrapping_add(cpu.x) as u16;
            let target_p1 = val.wrapping_add(cpu.x + 1) as u16;
            bus.write8(target, lo);
            bus.write8(target_p1, hi);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_and_izy() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![AND_IZY, val]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;

            bus.write8(val as u16, lo);
            bus.write8(val.wrapping_add(1) as u16, hi);

            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 & !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 6 } else { 5 });
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_eor_imm() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![EOR_IMM, val]);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_eor_zp() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![EOR_ZP, val]);
            bus.write8(val as u16, val);
            let mut cpu = Cpu::new();
            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 3);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_eor_zpx() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![EOR_ZPX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            bus.write8(val.wrapping_add(cpu.x) as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_eor_abs() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![EOR_ABS, val, 0xf0]);

            let mut cpu = Cpu::new();
            bus.write8(0xf000 + val as u16, !val);

            cpu.a = 0b1010_0101;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 4);
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_eor_abx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![EOR_ABX, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.x as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_eor_aby() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![EOR_ABY, lo, hi]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;
            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 5 } else { 4 });
            assert_eq!(cpu.pc - 0x8000, 3);
        }
    }

    #[test]
    fn test_eor_izx() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![EOR_IZX, val]);

            let mut cpu = Cpu::new();
            cpu.x = 0xf0;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            bus.write8(base, !val);

            let target = val.wrapping_add(cpu.x) as u16;
            let target_p1 = val.wrapping_add(cpu.x + 1) as u16;
            bus.write8(target, lo);
            bus.write8(target_p1, hi);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 6);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_eor_izy() {
        for val in 0..=255 {
            let lo = val;
            let hi = 0x02;

            let mut bus = Bus::new(0x8000, vec![EOR_IZY, val]);

            let mut cpu = Cpu::new();
            cpu.y = 0xf1;
            cpu.a = 0b1010_0101;

            let base = u16::from_le_bytes([lo, hi]);
            let target = base.wrapping_add(cpu.y as u16);
            let page_crossed = (base ^ target) & 0xff00 != 0;

            bus.write8(val as u16, lo);
            bus.write8(val.wrapping_add(1) as u16, hi);

            bus.write8(target, !val);

            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, 0b1010_0101 ^ !val);
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, if page_crossed { 6 } else { 5 });
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_adc_imm() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ADC_IMM, val]);
            let mut cpu = Cpu::new();
            let org_a = 0x0f;

            cpu.a = org_a;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, org_a.wrapping_add(val));
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_adc_carry_input() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![ADC_IMM, val]);
            let mut cpu = Cpu::new();
            let org_a = 0x0f;

            cpu.a = org_a;
            cpu.set_flag(flags::C, true);
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.a, org_a.wrapping_add(val).wrapping_add(1));
            assert_eq!(cpu.get_flag(flags::N), (cpu.a as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.a == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 2);
        }
    }

    #[test]
    fn test_adc_carry_output_when_no_carry() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 0xf0]);
        let mut cpu = Cpu::new();
        let org_a = 0x0f;

        cpu.a = org_a;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0xff);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc - 0x8000, 2);
    }

    #[test]
    fn test_adc_carry_1_and_neg1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 1]);
        let mut cpu = Cpu::new();
        let org_a = 0x0f;

        cpu.a = -1 as i8 as u8;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_adc_carry_neg128_and_neg1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 1]);
        let mut cpu = Cpu::new();
        let org_a = 0x0f;

        cpu.a = -1 as i8 as u8;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_adc_carry_output_when_carry() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 0x0f]);
        let mut cpu = Cpu::new();
        let org_a = 0xf2;

        cpu.a = org_a;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0x01);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc - 0x8000, 2);
    }

    #[test]
    fn test_adc_carry_output_when_carry_due_to_carry() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 0x0f]);
        let mut cpu = Cpu::new();
        let org_a = 0xf0;

        cpu.a = org_a;
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc - 0x8000, 2);
    }

    #[test]
    fn test_adc_overflow_none_positive() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 0x05]);
        let mut cpu = Cpu::new();
        let org_a = 0x0f;

        cpu.a = org_a;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0x14);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::V), false);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc - 0x8000, 2);
    }

    #[test]
    fn test_adc_overflow_none_negative() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, ((-2 as i8) as u8)]);
        let mut cpu = Cpu::new();
        let org_a = (-5 as i8) as u8;

        cpu.a = org_a;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, (-7 as i8) as u8);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::V), false);
        assert_eq!(cycles, 2);
        assert_eq!(cpu.pc - 0x8000, 2);
    }

    #[test]
    fn test_adc_overflow_none_1_and_1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 1]);
        let mut cpu = Cpu::new();

        cpu.a = 1;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 2);
        assert_eq!(cpu.get_flag(flags::V), false);
    }

    #[test]
    fn test_adc_overflow_none_1_and_neg1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 1]);
        let mut cpu = Cpu::new();

        cpu.a = -1 as i8 as u8;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_flag(flags::V), false);
    }

    #[test]
    fn test_adc_overflows_127_and_1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 127]);
        let mut cpu = Cpu::new();

        cpu.a = 1;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 128);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_adc_overflows_neg128_and_neg1() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, -128 as i8 as u8]);
        let mut cpu = Cpu::new();

        cpu.a = -1 as i8 as u8;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, -129 as i16 as u8);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_adc_overflows_63_and_64_and_carry_in() {
        let mut bus = Bus::new(0x8000, vec![ADC_IMM, 63]);
        let mut cpu = Cpu::new();

        cpu.a = 64;
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 128);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_sbc_0_less_1_not_overflows() {
        let mut bus = Bus::new(0x8000, vec![SBC_IMM, 1]);
        let mut cpu = Cpu::new();

        cpu.a = 0;
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, -1 as i8 as u8);
        assert_eq!(cpu.get_flag(flags::V), false);
    }

    #[test]
    fn test_sbc_neg128_less_1_overflows() {
        let mut bus = Bus::new(0x8000, vec![SBC_IMM, 1]);
        let mut cpu = Cpu::new();

        cpu.a = -128 as i16 as u8;
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, -129 as i16 as u8);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_sbc_127_less_neg1_overflows() {
        let mut bus = Bus::new(0x8000, vec![SBC_IMM, -1 as i8 as u8]);
        let mut cpu = Cpu::new();

        cpu.a = 127;
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 128);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_sbc_neg64_less_64_less_1() {
        let mut bus = Bus::new(0x8000, vec![SBC_IMM, 0x40]);
        let mut cpu = Cpu::new();

        cpu.a = 0xc0;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, -129 as i16 as u8);
        assert_eq!(cpu.get_flag(flags::V), true);
    }

    #[test]
    fn test_cmp_less_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CMP_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.a = 0x01;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0x01);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), false);
    }

    #[test]
    fn test_cmp_equal() {
        let mut bus = Bus::new(0x8000, vec![CMP_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.a = 0x02;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0x02);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_cmp_greater_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CMP_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.a = 0xF2;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.a, 0xF2);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_cpx_less_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CPX_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.x = 0x01;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.x, 0x01);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), false);
    }

    #[test]
    fn test_cpx_equal() {
        let mut bus = Bus::new(0x8000, vec![CPX_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.x = 0x02;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.x, 0x02);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_cpx_greater_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CPX_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.x = 0xF2;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.x, 0xF2);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_cpy_less_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CPY_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.x = 0x01;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.x, 0x01);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), false);
    }

    #[test]
    fn test_cpy_equal() {
        let mut bus = Bus::new(0x8000, vec![CPY_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.y = 0x02;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.y, 0x02);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_cpy_greater_with_negative() {
        let mut bus = Bus::new(0x8000, vec![CPY_IMM, 0x02]);
        let mut cpu = Cpu::new();

        cpu.y = 0xF2;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.y, 0xF2);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::C), true);
    }

    #[test]
    fn test_dec_abs() {
        let mut bus = Bus::new(0x8000, vec![DEC_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 12);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 11);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_inc_abs() {
        let mut bus = Bus::new(0x8000, vec![INC_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 12);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 13);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_dey() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![DEY_IMP]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val.wrapping_sub(1));
            assert_eq!(cpu.get_flag(flags::N), (cpu.y as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.y == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_iny() {
        for val in 0..=255 {
            let mut bus = Bus::new(0x8000, vec![INY_IMP]);
            let mut cpu = Cpu::new();
            cpu.y = val;
            let cycles = cpu.step(&mut bus);

            assert_eq!(cpu.y, val.wrapping_add(1));
            assert_eq!(cpu.get_flag(flags::N), (cpu.y as i8) < 0);
            assert_eq!(cpu.get_flag(flags::Z), cpu.y == 0);
            assert_eq!(cycles, 2);
            assert_eq!(cpu.pc - 0x8000, 1);
        }
    }

    #[test]
    fn test_asl_abs() {
        let mut bus = Bus::new(0x8000, vec![ASL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0100_0000);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b1000_0000);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_asl_abs_carry() {
        let mut bus = Bus::new(0x8000, vec![ASL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b1000_0000);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0000_0000);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_asl_abs_pattern() {
        let mut bus = Bus::new(0x8000, vec![ASL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0010_1010);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0101_0100);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_rol_abs() {
        let mut bus = Bus::new(0x8000, vec![ROL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0100_0000);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b1000_0001);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_rol_abs_carry_out() {
        let mut bus = Bus::new(0x8000, vec![ROL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b1000_0000);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0000_0000);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_rol_abs_carry_in_out() {
        let mut bus = Bus::new(0x8000, vec![ROL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b1000_0000);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0000_0001);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_rol_abs_pattern() {
        let mut bus = Bus::new(0x8000, vec![ROL_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0010_1010);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0101_0101);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_lsr_abs() {
        let mut bus = Bus::new(0x8000, vec![LSR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0100_0000);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0010_0000);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_lsr_abs_carry() {
        let mut bus = Bus::new(0x8000, vec![LSR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0000_0001);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0000_0000);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_lsr_abs_pattern() {
        let mut bus = Bus::new(0x8000, vec![LSR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0010_1010);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0001_0101);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_ror_abs() {
        let mut bus = Bus::new(0x8000, vec![ROR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0100_0000);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b1010_0000);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_ror_abs_carry_out() {
        let mut bus = Bus::new(0x8000, vec![ROR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0000_0001);

        let mut cpu = Cpu::new();
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b0000_0000);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), true);
        assert_eq!(cpu.get_flag(flags::N), false);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_ror_abs_carry_in_out() {
        let mut bus = Bus::new(0x8000, vec![ROR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b1000_0001);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b1100_0000);
        assert_eq!(cpu.get_flag(flags::C), true);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_ror_abs_pattern() {
        let mut bus = Bus::new(0x8000, vec![ROR_ABS, 0x02, 0x03]);
        bus.write8(0x0302, 0b0010_1010);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0302), 0b1001_0101);
        assert_eq!(cpu.get_flag(flags::C), false);
        assert_eq!(cpu.get_flag(flags::Z), false);
        assert_eq!(cpu.get_flag(flags::N), true);
        assert_eq!(cycles, 6);
        assert_eq!(cpu.pc - 0x8000, 3);
    }

    #[test]
    fn test_bpl_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BPL_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bpl_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BPL_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bpl_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BPL_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bmi_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BMI_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bmi_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BMI_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bmi_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BMI_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::N, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bvc_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BVC_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bvc_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BVC_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bvc_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BVC_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bvs_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BVS_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bvs_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BVS_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bvs_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BVS_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::V, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bcc_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BCC_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bcc_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BCC_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bcc_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BCC_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bcs_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BCS_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bcs_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BCS_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_bcs_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BCS_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::C, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bne_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BNE_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_bne_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BNE_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_bne_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BNE_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_beq_taken_same_page() {
        let mut bus = Bus::new(0x8000, vec![BEQ_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 20 + 2);
        assert_eq!(cycles, 3);
    }

    #[test]
    fn test_beq_not_taken() {
        let mut bus = Bus::new(0x8000, vec![BEQ_REL, 20]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, false);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 + 2);
        assert_eq!(cycles, 2);
    }

    #[test]
    fn test_beq_taken_diffrent_page() {
        let mut bus = Bus::new(0x8000, vec![BEQ_REL, -20 as i8 as u8]);

        let mut cpu = Cpu::new();
        cpu.set_flag(flags::Z, true);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8000 - 20 + 2);
        assert_eq!(cycles, 4);
    }

    #[test]
    fn test_brk() {
        let start_flags = 0b1110_1011;

        let mut bus = Bus::new(0x8123, vec![BRK_IMP]);
        bus.write8(vectors::IRQ, 0x34);
        bus.write8(vectors::IRQ.wrapping_add(1), 0x12);

        let mut cpu = Cpu::new();
        cpu.s = 0xff;
        cpu.pc = 0x8123;
        cpu.status = start_flags;
        let cycles = cpu.step(&mut bus);

        assert_eq!(bus.read8(0x0100 + cpu.s.wrapping_add(1) as u16), start_flags);
        assert_eq!(bus.read8(0x0100 + cpu.s.wrapping_add(2) as u16), 0x24);
        assert_eq!(bus.read8(0x0100 + cpu.s.wrapping_add(3) as u16), 0x81);
        assert_eq!(cpu.pc, 0x1234);
        assert_eq!(cpu.get_flag(flags::B), true);
        assert_eq!(cpu.get_flag(flags::I), true);
        assert_eq!(cycles, 7);
    }

    #[test]
    fn test_brk_rti() {
        let start_flags = 0b1110_1011;

        let mut bus = Bus::new(0x8123, vec![BRK_IMP]);
        bus.write8(vectors::IRQ, 0x34);
        bus.write8(vectors::IRQ.wrapping_add(1), 0x12);
        bus.write8(0x1234, RTI_IMP);

        let mut cpu = Cpu::new();
        cpu.s = 0xff;
        cpu.pc = 0x8123;
        cpu.status = start_flags;
        cpu.step(&mut bus);
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8124);
        assert_eq!(cpu.status, start_flags);
        assert_eq!(cycles, 6);
    }

    #[test]
    fn test_jsr_rts() {
        let mut bus = Bus::new(0x8122, vec![JSR_ABS, 0x21, 0x83]);
        bus.write8(0x8321, RTS_IMP);

        let mut cpu = Cpu::new();
        cpu.s = 0xff;
        cpu.pc = 0x8122;
        let cycles_jsr = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8321);
        assert_eq!(cpu.s, 0xFD);
        assert_eq!(bus.read8(0x0100 + cpu.s.wrapping_add(1) as u16), 0x24);
        assert_eq!(bus.read8(0x0100 + cpu.s.wrapping_add(2) as u16), 0x81);
        assert_eq!(cycles_jsr, 6);
        

        let cycles_rts = cpu.step(&mut bus);
        
        assert_eq!(cpu.pc, 0x8125);
        assert_eq!(cpu.s, 0xFF);
        assert_eq!(cycles_rts, 6);
    }

    #[test]
    fn test_jmp_abs() {
        let mut bus = Bus::new(0x8122, vec![JMP_ABS, 0x21, 0x83]);

        let mut cpu = Cpu::new();
        cpu.pc = 0x8122;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x8321);
        assert_eq!(cycles, 3);
    } 

    #[test]
    fn test_jmp_indirect() {
        let mut bus = Bus::new(0x8122, vec![JMP_IND, 0x21, 0x83]);
        bus.write8(0x8321, 0x44);
        bus.write8(0x8322, 0x55);

        let mut cpu = Cpu::new();
        cpu.pc = 0x8122;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x5544);
        assert_eq!(cycles, 5);
    } 

    #[test]
    fn test_jmp_indirect_bug() {
        let mut bus = Bus::new(0x8122, vec![JMP_IND, 0xFF, 0x83]);
        bus.write8(0x83FF, 0x44);
        bus.write8(0x8300, 0x55);

        let mut cpu = Cpu::new();
        cpu.pc = 0x8122;
        let cycles = cpu.step(&mut bus);

        assert_eq!(cpu.pc, 0x5544);
        assert_eq!(cycles, 5);
    } 
}
