use crate::bus::Bus;
use bitflags::bitflags;
use lazy_static::lazy_static;
use std::collections::HashMap;

const STACK_BASE: u16 = 0x100;

type OpcodeMethod = fn(&mut CPU, &AddressingMode);

#[rustfmt::skip]
lazy_static! {
    // Instruction reference: https://www.nesdev.org/obelisk-6502-guide/reference.html
    pub static ref OP_CODES_MAP: HashMap<u8, Instruction> = {
        HashMap::from([
            (0x00, Instruction::new(7, &(CPU::brk as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xea, Instruction::new(2, &(CPU::nop as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x18, Instruction::new(2, &(CPU::clc as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x38, Instruction::new(2, &(CPU::sec as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xd8, Instruction::new(2, &(CPU::cld as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xf8, Instruction::new(2, &(CPU::sed as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xb8, Instruction::new(2, &(CPU::clv as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x58, Instruction::new(2, &(CPU::cli as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x78, Instruction::new(2, &(CPU::sei as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xaa, Instruction::new(2, &(CPU::tax as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x8a, Instruction::new(2, &(CPU::txa as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x9a, Instruction::new(2, &(CPU::txs as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xba, Instruction::new(2, &(CPU::tsx as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0x20, Instruction::new(6, &(CPU::jsr as OpcodeMethod), AddressingMode::Absolute)),
            (0xe8, Instruction::new(2, &(CPU::inx as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xc8, Instruction::new(2, &(CPU::iny as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x60, Instruction::new(6, &(CPU::rts as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0xe6, Instruction::new(5, &(CPU::inc as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xf6, Instruction::new(6, &(CPU::inc as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xee, Instruction::new(6, &(CPU::inc as OpcodeMethod), AddressingMode::Absolute)),
            (0xfe, Instruction::new(7, &(CPU::inc as OpcodeMethod), AddressingMode::AbsoluteX)),

            (0xa9, Instruction::new(2, &(CPU::lda as OpcodeMethod), AddressingMode::Immediate)),
            (0xa5, Instruction::new(3, &(CPU::lda as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xb5, Instruction::new(4, &(CPU::lda as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xad, Instruction::new(4, &(CPU::lda as OpcodeMethod), AddressingMode::Absolute)),
            (0xbd, Instruction::new(4, &(CPU::lda as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0xb9, Instruction::new(4, &(CPU::lda as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0xa1, Instruction::new(6, &(CPU::lda as OpcodeMethod), AddressingMode::IndirectX)),
            (0xb1, Instruction::new(5, &(CPU::lda as OpcodeMethod), AddressingMode::IndirectY)),

            (0xa2, Instruction::new(2, &(CPU::ldx as OpcodeMethod), AddressingMode::Immediate)),
            (0xa6, Instruction::new(3, &(CPU::ldx as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xb6, Instruction::new(4, &(CPU::ldx as OpcodeMethod), AddressingMode::ZeroPageY)),
            (0xae, Instruction::new(4, &(CPU::ldx as OpcodeMethod), AddressingMode::Absolute)),
            (0xbe, Instruction::new(4, &(CPU::ldx as OpcodeMethod), AddressingMode::AbsoluteY)),

            (0xa0, Instruction::new(2, &(CPU::ldy as OpcodeMethod), AddressingMode::Immediate)),
            (0xa4, Instruction::new(3, &(CPU::ldy as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xb4, Instruction::new(4, &(CPU::ldy as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xac, Instruction::new(4, &(CPU::ldy as OpcodeMethod), AddressingMode::Absolute)),
            (0xbc, Instruction::new(4, &(CPU::ldy as OpcodeMethod), AddressingMode::AbsoluteX)),

            (0x85, Instruction::new(3, &(CPU::sta as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x95, Instruction::new(4, &(CPU::sta as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x8d, Instruction::new(4, &(CPU::sta as OpcodeMethod), AddressingMode::Absolute)),
            (0x9d, Instruction::new(5, &(CPU::sta as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0x99, Instruction::new(5, &(CPU::sta as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0x81, Instruction::new(6, &(CPU::sta as OpcodeMethod), AddressingMode::IndirectX)),
            (0x91, Instruction::new(6, &(CPU::sta as OpcodeMethod), AddressingMode::IndirectY)),

            (0x29, Instruction::new(2, &(CPU::and as OpcodeMethod), AddressingMode::Immediate)),
            (0x25, Instruction::new(3, &(CPU::and as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x35, Instruction::new(4, &(CPU::and as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x2D, Instruction::new(4, &(CPU::and as OpcodeMethod), AddressingMode::Absolute)),
            (0x3D, Instruction::new(4, &(CPU::and as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0x39, Instruction::new(4, &(CPU::and as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0x21, Instruction::new(6, &(CPU::and as OpcodeMethod), AddressingMode::IndirectX)),
            (0x31, Instruction::new(5, &(CPU::and as OpcodeMethod), AddressingMode::IndirectY)),

            (0x49, Instruction::new(2, &(CPU::eor as OpcodeMethod), AddressingMode::Immediate)),
            (0x45, Instruction::new(3, &(CPU::eor as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x55, Instruction::new(4, &(CPU::eor as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x4D, Instruction::new(4, &(CPU::eor as OpcodeMethod), AddressingMode::Absolute)),
            (0x5D, Instruction::new(4, &(CPU::eor as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0x59, Instruction::new(4, &(CPU::eor as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0x41, Instruction::new(6, &(CPU::eor as OpcodeMethod), AddressingMode::IndirectX)),
            (0x51, Instruction::new(5, &(CPU::eor as OpcodeMethod), AddressingMode::IndirectY)),

            (0x09, Instruction::new(2, &(CPU::ora as OpcodeMethod), AddressingMode::Immediate)),
            (0x05, Instruction::new(3, &(CPU::ora as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x15, Instruction::new(4, &(CPU::ora as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x0d, Instruction::new(4, &(CPU::ora as OpcodeMethod), AddressingMode::Absolute)),
            (0x1d, Instruction::new(4, &(CPU::ora as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0x19, Instruction::new(4, &(CPU::ora as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0x01, Instruction::new(6, &(CPU::ora as OpcodeMethod), AddressingMode::IndirectX)),
            (0x11, Instruction::new(5, &(CPU::ora as OpcodeMethod), AddressingMode::IndirectY)),

            (0x69, Instruction::new(2, &(CPU::adc as OpcodeMethod), AddressingMode::Immediate)),
            (0x65, Instruction::new(3, &(CPU::adc as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x75, Instruction::new(4, &(CPU::adc as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x6d, Instruction::new(4, &(CPU::adc as OpcodeMethod), AddressingMode::Absolute)),
            (0x7d, Instruction::new(4, &(CPU::adc as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0x79, Instruction::new(4, &(CPU::adc as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0x61, Instruction::new(6, &(CPU::adc as OpcodeMethod), AddressingMode::IndirectX)),
            (0x71, Instruction::new(5, &(CPU::adc as OpcodeMethod), AddressingMode::IndirectY)),

            (0xe9, Instruction::new(2, &(CPU::sbc as OpcodeMethod), AddressingMode::Immediate)),
            (0xe5, Instruction::new(3, &(CPU::sbc as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xf5, Instruction::new(4, &(CPU::sbc as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xed, Instruction::new(4, &(CPU::sbc as OpcodeMethod), AddressingMode::Absolute)),
            (0xfd, Instruction::new(4, &(CPU::sbc as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0xf9, Instruction::new(4, &(CPU::sbc as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0xe1, Instruction::new(6, &(CPU::sbc as OpcodeMethod), AddressingMode::IndirectX)),
            (0xf1, Instruction::new(5, &(CPU::sbc as OpcodeMethod), AddressingMode::IndirectY)),

            (0xc9, Instruction::new(2, &(CPU::cmp as OpcodeMethod), AddressingMode::Immediate)),
            (0xc5, Instruction::new(3, &(CPU::cmp as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xd5, Instruction::new(4, &(CPU::cmp as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xcd, Instruction::new(4, &(CPU::cmp as OpcodeMethod), AddressingMode::Absolute)),
            (0xdd, Instruction::new(4, &(CPU::cmp as OpcodeMethod), AddressingMode::AbsoluteX)),
            (0xd9, Instruction::new(4, &(CPU::cmp as OpcodeMethod), AddressingMode::AbsoluteY)),
            (0xc1, Instruction::new(6, &(CPU::cmp as OpcodeMethod), AddressingMode::IndirectX)),
            (0xd1, Instruction::new(5, &(CPU::cmp as OpcodeMethod), AddressingMode::IndirectY)),

            (0xe0, Instruction::new(2, &(CPU::cpx as OpcodeMethod), AddressingMode::Immediate)),
            (0xe4, Instruction::new(3, &(CPU::cpx as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xec, Instruction::new(4, &(CPU::cpx as OpcodeMethod), AddressingMode::Absolute)),

            (0xc0, Instruction::new(2, &(CPU::cpy as OpcodeMethod), AddressingMode::Immediate)),
            (0xc4, Instruction::new(3, &(CPU::cpy as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xcc, Instruction::new(4, &(CPU::cpy as OpcodeMethod), AddressingMode::Absolute)),

            (0xf0, Instruction::new(2, &(CPU::beq as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xd0, Instruction::new(2, &(CPU::bne as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0xb0, Instruction::new(2, &(CPU::bcs as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x90, Instruction::new(2, &(CPU::bcc as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0x10, Instruction::new(2, &(CPU::bpl as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x30, Instruction::new(2, &(CPU::bmi as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0xc6, Instruction::new(5, &(CPU::dec as OpcodeMethod), AddressingMode::ZeroPage)),
            (0xd6, Instruction::new(6, &(CPU::dec as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0xce, Instruction::new(6, &(CPU::dec as OpcodeMethod), AddressingMode::Absolute)),
            (0xde, Instruction::new(7, &(CPU::dec as OpcodeMethod), AddressingMode::AbsoluteX)),

            (0xca, Instruction::new(2, &(CPU::dex as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x88, Instruction::new(2, &(CPU::dey as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0x4a, Instruction::new(2, &(CPU::lsr as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x46, Instruction::new(5, &(CPU::lsr as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x56, Instruction::new(6, &(CPU::lsr as OpcodeMethod), AddressingMode::ZeroPageX)),
            (0x4e, Instruction::new(6, &(CPU::lsr as OpcodeMethod), AddressingMode::Absolute)),
            (0x5e, Instruction::new(7, &(CPU::lsr as OpcodeMethod), AddressingMode::AbsoluteX)),

            (0x24, Instruction::new(3, &(CPU::bit as OpcodeMethod), AddressingMode::ZeroPage)),
            (0x2c, Instruction::new(4, &(CPU::bit as OpcodeMethod), AddressingMode::Absolute)),

            (0x4c, Instruction::new(3, &(CPU::jmp as OpcodeMethod), AddressingMode::Absolute)),
            (0x6c, Instruction::new(5, &(CPU::jmp as OpcodeMethod), AddressingMode::Indirect)),

            (0x48, Instruction::new(3, &(CPU::pha as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x68, Instruction::new(4, &(CPU::pla as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0x50, Instruction::new(2, &(CPU::bvc as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x70, Instruction::new(2, &(CPU::bvs as OpcodeMethod), AddressingMode::NoneAddressing)),

            (0x08, Instruction::new(3, &(CPU::php as OpcodeMethod), AddressingMode::NoneAddressing)),
            (0x28, Instruction::new(4, &(CPU::plp as OpcodeMethod), AddressingMode::NoneAddressing)),
        ])
    };
}

bitflags! {
    struct StatusFlags: u8 {
        const CARRY             = 1 << 0;
        const ZERO              = 1 << 1;
        const INTERRUPT_DISABLE = 1 << 2;
        // No CPU effect
        const DECIMAL_MODE      = 1 << 3;
        const BREAK_COMMAND     = 1 << 4;
        // No CPU effect; always pushed as 1
        const UNUSED            = 1 << 5;
        const OVERFLOW          = 1 << 6;
        const NEGATIVE          = 1 << 7;
    }
}

pub struct Instruction {
    method: OpcodeMethod,
    #[allow(dead_code)]
    cycles: u8,
    addressing_mode: AddressingMode,
}

impl Instruction {
    fn new(cycles: u8, method: &OpcodeMethod, addressing_mode: AddressingMode) -> Self {
        Self {
            method: *method,
            cycles,
            addressing_mode,
        }
    }
}

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    NoneAddressing,
}

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: StatusFlags,
    stack_pointer: u8,
    program_counter: u16,
    bus: Bus,
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: StatusFlags::from_bits_truncate(0b0010_0000),
            stack_pointer: 0xff,
            program_counter: 0,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = StatusFlags::from_bits_truncate(0b0010_0000);
        self.program_counter = self.peek_u16(0xfffc);
    }

    pub fn peek_u8(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    pub fn read_u8(&mut self) -> u8 {
        let value = self.peek_u8(self.program_counter);
        self.program_counter = self.program_counter.wrapping_add(1);
        value
    }

    fn read_i8(&mut self) -> i8 {
        self.read_u8() as i8
    }

    fn read_u16(&mut self) -> u16 {
        u16::from_le_bytes([self.read_u8(), self.read_u8()])
    }

    fn peek_u16(&mut self, addr: u16) -> u16 {
        u16::from_le_bytes([self.peek_u8(addr), self.peek_u8(addr.wrapping_add(1))])
    }

    pub fn write_u8(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    #[allow(dead_code)]
    fn write_u16(&mut self, addr: u16, data: u16) {
        let [lo, hi] = u16::to_le_bytes(data);
        self.write_u8(addr, lo);
        self.write_u8(addr + 1, hi);
    }

    fn stack_push_u8(&mut self, data: u8) {
        self.bus
            .mem_write(self.stack_pointer as u16 + STACK_BASE, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u8(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.bus.mem_read(self.stack_pointer as u16 + STACK_BASE)
    }

    fn stack_push_u16(&mut self, data: u16) {
        let [lo, hi] = u16::to_le_bytes(data);
        self.stack_push_u8(hi);
        self.stack_push_u8(lo);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        u16::from_le_bytes([self.stack_pop_u8(), self.stack_pop_u8()])
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        // Update zero flag.
        self.status.set(StatusFlags::ZERO, result == 0);

        // Update negative flag.
        self.status
            .set(StatusFlags::NEGATIVE, result & 0b1000_0000 != 0);
    }

    fn operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => {
                let addr = self.program_counter;
                self.program_counter += 1;
                addr
            }
            AddressingMode::ZeroPage => self.read_u8().into(),
            AddressingMode::ZeroPageX => self.read_u8().wrapping_add(self.register_x).into(),
            AddressingMode::ZeroPageY => self.read_u8().wrapping_add(self.register_y).into(),
            AddressingMode::Absolute => self.read_u16(),
            AddressingMode::AbsoluteX => self.read_u16().wrapping_add(self.register_x.into()),
            AddressingMode::AbsoluteY => self.read_u16().wrapping_add(self.register_y.into()),
            AddressingMode::Indirect => {
                todo!()
            }
            AddressingMode::IndirectX => {
                let ptr = self.read_u8().wrapping_add(self.register_x);
                u16::from_le_bytes([
                    self.bus.mem_read(ptr.into()),
                    self.bus.mem_read(ptr.wrapping_add(1).into()),
                ])
            }
            AddressingMode::IndirectY => {
                let base = self.read_u8();
                u16::from_le_bytes([
                    self.bus.mem_read(base.into()),
                    self.bus.mem_read(base.wrapping_add(1).into()),
                ])
                .wrapping_add(self.register_y.into())
            }
            AddressingMode::NoneAddressing => panic!("mode {:?} is not supported", mode),
        }
    }

    #[cfg(test)]
    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            if self.status.contains(StatusFlags::BREAK_COMMAND) {
                return;
            }

            callback(self);

            let opcode = self.read_u8();
            let opcode = match OP_CODES_MAP.get(&opcode) {
                Some(opcode) => opcode,
                None => todo!("{}", format!("opcode {:#02x?}", opcode)),
            };

            (opcode.method)(self, &opcode.addressing_mode);
        }
    }

    fn brk(&mut self, _mode: &AddressingMode) {
        self.status.insert(StatusFlags::BREAK_COMMAND);
    }

    fn nop(&mut self, _mode: &AddressingMode) {}

    fn clv(&mut self, _mode: &AddressingMode) {
        self.status.remove(StatusFlags::OVERFLOW);
    }

    fn cli(&mut self, _mode: &AddressingMode) {
        self.status.remove(StatusFlags::INTERRUPT_DISABLE);
    }

    fn sei(&mut self, _mode: &AddressingMode) {
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
    }

    fn cld(&mut self, _mode: &AddressingMode) {
        self.status.remove(StatusFlags::DECIMAL_MODE);
    }

    fn sed(&mut self, _mode: &AddressingMode) {
        self.status.insert(StatusFlags::DECIMAL_MODE);
    }

    fn clc(&mut self, _mode: &AddressingMode) {
        self.status.remove(StatusFlags::CARRY);
    }

    fn sec(&mut self, _mode: &AddressingMode) {
        self.status.insert(StatusFlags::CARRY);
    }

    fn iny(&mut self, _mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn inx(&mut self, _mode: &AddressingMode) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr).wrapping_add(1);
        self.write_u8(addr, value);
        self.update_zero_and_negative_flags(value);
    }

    fn txa(&mut self, _mode: &AddressingMode) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn tax(&mut self, _mode: &AddressingMode) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self, _mode: &AddressingMode) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dex(&mut self, _mode: &AddressingMode) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr).wrapping_sub(1);
        self.write_u8(addr, value);
        self.update_zero_and_negative_flags(value);
    }

    fn branch(&mut self, condition: bool) {
        let value = self.read_i8();
        if condition {
            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
        }
    }

    fn bvs(&mut self, _mode: &AddressingMode) {
        self.branch(self.status.contains(StatusFlags::OVERFLOW));
    }

    fn bvc(&mut self, _mode: &AddressingMode) {
        self.branch(!self.status.contains(StatusFlags::OVERFLOW));
    }

    fn bcc(&mut self, _mode: &AddressingMode) {
        self.branch(!self.status.contains(StatusFlags::CARRY));
    }

    fn bcs(&mut self, _mode: &AddressingMode) {
        self.branch(self.status.contains(StatusFlags::CARRY));
    }

    fn bmi(&mut self, _mode: &AddressingMode) {
        self.branch(self.status.contains(StatusFlags::NEGATIVE));
    }

    fn bpl(&mut self, _mode: &AddressingMode) {
        self.branch(!self.status.contains(StatusFlags::NEGATIVE));
    }

    fn bne(&mut self, _mode: &AddressingMode) {
        self.branch(!self.status.contains(StatusFlags::ZERO));
    }

    fn beq(&mut self, _mode: &AddressingMode) {
        self.branch(self.status.contains(StatusFlags::ZERO));
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        self.program_counter = self.operand_address(mode);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr);
        self.status
            .set(StatusFlags::OVERFLOW, value & 0b0100_0000 == 1);
        self.status
            .set(StatusFlags::NEGATIVE, value & 0b1000_0000 == 1);
        if value & self.register_a == 0 {
            self.status.insert(StatusFlags::ZERO);
        }
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr);
        let result = self.register_y.wrapping_sub(value);
        if self.register_y >= self.peek_u8(addr) {
            self.status.insert(StatusFlags::CARRY);
        }

        self.update_zero_and_negative_flags(result);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr);
        let result = self.register_x.wrapping_sub(value);
        if self.register_x >= value {
            self.status.insert(StatusFlags::CARRY);
        }

        self.update_zero_and_negative_flags(result);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let value = self.peek_u8(addr);
        let result = self.register_a.wrapping_sub(value);
        if self.register_a >= value {
            self.status.insert(StatusFlags::CARRY);
        }

        self.update_zero_and_negative_flags(result);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let result = match mode {
            AddressingMode::NoneAddressing => {
                self.status
                    .set(StatusFlags::CARRY, self.register_a & 0b0000_0001 == 1);
                self.register_a = self.register_a.wrapping_shr(1);
                self.register_a
            }
            _ => {
                let addr = self.operand_address(mode);
                let value = self.bus.mem_read(addr);
                self.status
                    .set(StatusFlags::CARRY, value & 0b0000_0001 == 1);
                let result = value.wrapping_shr(1);
                self.write_u8(addr, result);
                result
            }
        };

        self.update_zero_and_negative_flags(result);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let (value, overflow1) = self.register_a.overflowing_sub(self.peek_u8(addr));
        let (result, overflow2) =
            value.overflowing_sub(!self.status.contains(StatusFlags::CARRY) as u8);
        if overflow1 || overflow2 {
            self.status.remove(StatusFlags::CARRY);
        }
        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        let (value, overflow1) = self.register_a.overflowing_add(self.peek_u8(addr));
        let (result, overflow2) =
            value.overflowing_add(self.status.contains(StatusFlags::CARRY) as u8);
        self.register_a = result;
        self.status.set(StatusFlags::CARRY, overflow1 || overflow2);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_a = self.register_a ^ self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_a = self.register_a | self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_a = self.register_a & self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rts(&mut self, _mode: &AddressingMode) {
        self.program_counter = self.stack_pop_u16()
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.stack_push_u16(self.program_counter);
        self.program_counter = addr;
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.write_u8(addr, self.register_a);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_y = self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_x = self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.operand_address(mode);
        self.register_a = self.peek_u8(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pla(&mut self, _mode: &AddressingMode) {
        self.register_a = self.peek_u8(self.stack_pointer as u16 + STACK_BASE);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self, _mode: &AddressingMode) {
        self.write_u8(self.stack_pointer as u16 + STACK_BASE, self.register_a);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn plp(&mut self, _mode: &AddressingMode) {
        self.status = StatusFlags::from_bits_truncate(self.stack_pop_u8());
    }

    fn php(&mut self, _mode: &AddressingMode) {
        self.stack_push_u8(self.status.bits());
    }

    fn txs(&mut self, _mode: &AddressingMode) {
        self.stack_pointer = self.register_x;
    }

    fn tsx(&mut self, _mode: &AddressingMode) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }
}

#[cfg(test)]
mod test {
    use crate::bus::Bus;
    use crate::cpu::{StatusFlags, CPU};
    use crate::rom::tests::test_rom;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let bus = Bus::new(test_rom(vec![0xa9, 0x05, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(StatusFlags::ZERO));
        assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let bus = Bus::new(test_rom(vec![0xa9, 0x00, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let bus = Bus::new(test_rom(vec![0xa9, 0x0a, 0xaa, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let bus = Bus::new(test_rom(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let bus = Bus::new(test_rom(vec![0xa2, 0xff, 0xe8, 0xe8, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let bus = Bus::new(test_rom(vec![0xa5, 0x10, 0x00]));
        let mut cpu = CPU::new(bus);
        cpu.write_u8(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_subroutines_with_inx() {
        let bus = Bus::new(test_rom(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x60,
        ]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 3);
    }

    #[test]
    fn test_nested_subroutines() {
        let bus = Bus::new(test_rom(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x20, 0x0f, 0x06, 0x60,
            0x00, 0xe8, 0x60, 0x00,
        ]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 5);
    }

    #[test]
    fn test_and() {
        let bus = Bus::new(test_rom(vec![0xa9, 0x10, 0x29, 0x30]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x10);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_and_zero() {
        let bus = Bus::new(test_rom(vec![0xa9, 0x1, 0x29, 0x2]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0);
        assert!(!cpu.status.is_empty());
    }

    #[test]
    fn test_adc() {
        let bus = Bus::new(test_rom(vec![0xa9, 0xff, 0x69, 0x1, 0x69, 0x1]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 2);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_dex_bpl() {
        let bus = Bus::new(test_rom(vec![0xa2, 0x4, 0xca, 0xca, 0x10, 0xfd]));
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 0xff);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }
}
