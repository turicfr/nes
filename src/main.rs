use bitflags::bitflags;
use lazy_static::lazy_static;
use rand::{thread_rng, Rng};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::EventPump;
use std::cmp::PartialEq;
use std::collections::HashMap;

const STACK_BASE: u16 = 0x100;

#[rustfmt::skip]
lazy_static! {
    pub static ref OP_CODES_MAP: HashMap<u8, OpCodeInfo> = {
        HashMap::from([
            (0x00, OpCodeInfo::new(OpCode::BRK, 1, 7, AddressingMode::NoneAddressing)),
            (0xea, OpCodeInfo::new(OpCode::NOP, 1, 2, AddressingMode::NoneAddressing)),
            (0x18, OpCodeInfo::new(OpCode::CLC, 1, 2, AddressingMode::NoneAddressing)),
            (0x38, OpCodeInfo::new(OpCode::SEC, 1, 2, AddressingMode::NoneAddressing)),
            (0xd8, OpCodeInfo::new(OpCode::CLD, 1, 2, AddressingMode::NoneAddressing)),
            (0xf8, OpCodeInfo::new(OpCode::SED, 1, 2, AddressingMode::NoneAddressing)),
            (0xb8, OpCodeInfo::new(OpCode::CLV, 1, 2, AddressingMode::NoneAddressing)),
            (0x58, OpCodeInfo::new(OpCode::CLI, 1, 2, AddressingMode::NoneAddressing)),
            (0x78, OpCodeInfo::new(OpCode::SEI, 1, 2, AddressingMode::NoneAddressing)),
            (0xaa, OpCodeInfo::new(OpCode::TAX, 1, 2, AddressingMode::NoneAddressing)),
            (0x8a, OpCodeInfo::new(OpCode::TXA, 1, 2, AddressingMode::NoneAddressing)),
            (0x20, OpCodeInfo::new(OpCode::JSR, 3, 6, AddressingMode::Absolute)),
            (0xe8, OpCodeInfo::new(OpCode::INX, 1, 2, AddressingMode::NoneAddressing)),
            (0xc8, OpCodeInfo::new(OpCode::INY, 1, 2, AddressingMode::NoneAddressing)),
            (0x60, OpCodeInfo::new(OpCode::RTS, 1, 6, AddressingMode::NoneAddressing)),

            (0xe6, OpCodeInfo::new(OpCode::INC, 2, 5, AddressingMode::ZeroPage)),
            (0xf6, OpCodeInfo::new(OpCode::INC, 2, 6, AddressingMode::ZeroPageX)),
            (0xee, OpCodeInfo::new(OpCode::INC, 3, 6, AddressingMode::Absolute)),
            (0xfe, OpCodeInfo::new(OpCode::INC, 2, 7, AddressingMode::AbsoluteX)),

            (0xa9, OpCodeInfo::new(OpCode::LDA, 2, 2, AddressingMode::Immediate)),
            (0xa5, OpCodeInfo::new(OpCode::LDA, 2, 3, AddressingMode::ZeroPage)),
            (0xb5, OpCodeInfo::new(OpCode::LDA, 2, 4, AddressingMode::ZeroPageX)),
            (0xad, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::Absolute)),
            (0xbd, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::AbsoluteX)),
            (0xb9, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::AbsoluteY)),
            (0xa1, OpCodeInfo::new(OpCode::LDA, 2, 6, AddressingMode::IndirectX)),
            (0xb1, OpCodeInfo::new(OpCode::LDA, 2, 5, AddressingMode::IndirectY)),

            (0xa2, OpCodeInfo::new(OpCode::LDX, 2, 2, AddressingMode::Immediate)),
            (0xa6, OpCodeInfo::new(OpCode::LDX, 2, 3, AddressingMode::ZeroPage)),
            (0xb6, OpCodeInfo::new(OpCode::LDX, 2, 4, AddressingMode::ZeroPageY)),
            (0xae, OpCodeInfo::new(OpCode::LDX, 3, 4, AddressingMode::Absolute)),
            (0xbe, OpCodeInfo::new(OpCode::LDX, 3, 4, AddressingMode::AbsoluteY)),

            (0xa0, OpCodeInfo::new(OpCode::LDY, 2, 2, AddressingMode::Immediate)),
            (0xa4, OpCodeInfo::new(OpCode::LDY, 2, 3, AddressingMode::ZeroPage)),
            (0xb4, OpCodeInfo::new(OpCode::LDY, 2, 4, AddressingMode::ZeroPageX)),
            (0xac, OpCodeInfo::new(OpCode::LDY, 3, 4, AddressingMode::Absolute)),
            (0xbc, OpCodeInfo::new(OpCode::LDY, 3, 4, AddressingMode::AbsoluteX)),

            (0x85, OpCodeInfo::new(OpCode::STA, 2, 3, AddressingMode::ZeroPage)),
            (0x95, OpCodeInfo::new(OpCode::STA, 2, 4, AddressingMode::ZeroPageX)),
            (0x8d, OpCodeInfo::new(OpCode::STA, 3, 4, AddressingMode::Absolute)),
            (0x9d, OpCodeInfo::new(OpCode::STA, 3, 5, AddressingMode::AbsoluteX)),
            (0x99, OpCodeInfo::new(OpCode::STA, 3, 5, AddressingMode::AbsoluteY)),
            (0x81, OpCodeInfo::new(OpCode::STA, 2, 6, AddressingMode::IndirectX)),
            (0x91, OpCodeInfo::new(OpCode::STA, 2, 6, AddressingMode::IndirectY)),

            (0x29, OpCodeInfo::new(OpCode::AND, 2, 2, AddressingMode::Immediate)),
            (0x25, OpCodeInfo::new(OpCode::AND, 2, 3, AddressingMode::ZeroPage)),
            (0x35, OpCodeInfo::new(OpCode::AND, 2, 4, AddressingMode::ZeroPageX)),
            (0x2D, OpCodeInfo::new(OpCode::AND, 3, 4, AddressingMode::Absolute)),
            (0x3D, OpCodeInfo::new(OpCode::AND, 3, 4, AddressingMode::AbsoluteX)),
            (0x39, OpCodeInfo::new(OpCode::AND, 3, 4, AddressingMode::AbsoluteY)),
            (0x21, OpCodeInfo::new(OpCode::AND, 2, 6, AddressingMode::IndirectX)),
            (0x31, OpCodeInfo::new(OpCode::AND, 2, 5, AddressingMode::IndirectY)),

            (0x49, OpCodeInfo::new(OpCode::EOR, 2, 2, AddressingMode::Immediate)),
            (0x45, OpCodeInfo::new(OpCode::EOR, 2, 3, AddressingMode::ZeroPage)),
            (0x55, OpCodeInfo::new(OpCode::EOR, 2, 4, AddressingMode::ZeroPageX)),
            (0x4D, OpCodeInfo::new(OpCode::EOR, 3, 4, AddressingMode::Absolute)),
            (0x5D, OpCodeInfo::new(OpCode::EOR, 3, 4, AddressingMode::AbsoluteX)),
            (0x59, OpCodeInfo::new(OpCode::EOR, 3, 4, AddressingMode::AbsoluteY)),
            (0x41, OpCodeInfo::new(OpCode::EOR, 2, 6, AddressingMode::IndirectX)),
            (0x51, OpCodeInfo::new(OpCode::EOR, 2, 5, AddressingMode::IndirectY)),

            (0x09, OpCodeInfo::new(OpCode::ORA, 2, 2, AddressingMode::Immediate)),
            (0x05, OpCodeInfo::new(OpCode::ORA, 2, 3, AddressingMode::ZeroPage)),
            (0x15, OpCodeInfo::new(OpCode::ORA, 2, 4, AddressingMode::ZeroPageX)),
            (0x0d, OpCodeInfo::new(OpCode::ORA, 3, 4, AddressingMode::Absolute)),
            (0x1d, OpCodeInfo::new(OpCode::ORA, 3, 4, AddressingMode::AbsoluteX)),
            (0x19, OpCodeInfo::new(OpCode::ORA, 3, 4, AddressingMode::AbsoluteY)),
            (0x01, OpCodeInfo::new(OpCode::ORA, 2, 6, AddressingMode::IndirectX)),
            (0x11, OpCodeInfo::new(OpCode::ORA, 2, 5, AddressingMode::IndirectY)),

            (0x69, OpCodeInfo::new(OpCode::ADC, 2, 2, AddressingMode::Immediate)),
            (0x65, OpCodeInfo::new(OpCode::ADC, 2, 3, AddressingMode::ZeroPage)),
            (0x75, OpCodeInfo::new(OpCode::ADC, 2, 4, AddressingMode::ZeroPageX)),
            (0x6d, OpCodeInfo::new(OpCode::ADC, 3, 4, AddressingMode::Absolute)),
            (0x7d, OpCodeInfo::new(OpCode::ADC, 3, 4, AddressingMode::AbsoluteX)),
            (0x79, OpCodeInfo::new(OpCode::ADC, 3, 4, AddressingMode::AbsoluteY)),
            (0x61, OpCodeInfo::new(OpCode::ADC, 2, 6, AddressingMode::IndirectX)),
            (0x71, OpCodeInfo::new(OpCode::ADC, 2, 5, AddressingMode::IndirectY)),

            (0xe9, OpCodeInfo::new(OpCode::SBC, 2, 2, AddressingMode::Immediate)),
            (0xe5, OpCodeInfo::new(OpCode::SBC, 2, 3, AddressingMode::ZeroPage)),
            (0xf5, OpCodeInfo::new(OpCode::SBC, 2, 4, AddressingMode::ZeroPageX)),
            (0xed, OpCodeInfo::new(OpCode::SBC, 3, 4, AddressingMode::Absolute)),
            (0xfd, OpCodeInfo::new(OpCode::SBC, 3, 4, AddressingMode::AbsoluteX)),
            (0xf9, OpCodeInfo::new(OpCode::SBC, 3, 4, AddressingMode::AbsoluteY)),
            (0xe1, OpCodeInfo::new(OpCode::SBC, 2, 6, AddressingMode::IndirectX)),
            (0xf1, OpCodeInfo::new(OpCode::SBC, 2, 5, AddressingMode::IndirectY)),

            (0xc9, OpCodeInfo::new(OpCode::CMP, 2, 2, AddressingMode::Immediate)),
            (0xc5, OpCodeInfo::new(OpCode::CMP, 2, 3, AddressingMode::ZeroPage)),
            (0xd5, OpCodeInfo::new(OpCode::CMP, 2, 4, AddressingMode::ZeroPageX)),
            (0xcd, OpCodeInfo::new(OpCode::CMP, 3, 4, AddressingMode::Absolute)),
            (0xdd, OpCodeInfo::new(OpCode::CMP, 3, 4, AddressingMode::AbsoluteX)),
            (0xd9, OpCodeInfo::new(OpCode::CMP, 3, 4, AddressingMode::AbsoluteY)),
            (0xc1, OpCodeInfo::new(OpCode::CMP, 2, 6, AddressingMode::IndirectX)),
            (0xd1, OpCodeInfo::new(OpCode::CMP, 2, 5, AddressingMode::IndirectY)),

            (0xe0, OpCodeInfo::new(OpCode::CPX, 2, 2, AddressingMode::Immediate)),
            (0xe4, OpCodeInfo::new(OpCode::CPX, 2, 3, AddressingMode::ZeroPage)),
            (0xec, OpCodeInfo::new(OpCode::CPX, 3, 4, AddressingMode::Absolute)),

            (0xc0, OpCodeInfo::new(OpCode::CPY, 2, 2, AddressingMode::Immediate)),
            (0xc4, OpCodeInfo::new(OpCode::CPY, 2, 3, AddressingMode::ZeroPage)),
            (0xcc, OpCodeInfo::new(OpCode::CPY, 3, 4, AddressingMode::Absolute)),

            (0xf0, OpCodeInfo::new(OpCode::BEQ, 2, 2, AddressingMode::NoneAddressing)),
            (0xd0, OpCodeInfo::new(OpCode::BNE, 2, 2, AddressingMode::NoneAddressing)),
            (0xb0, OpCodeInfo::new(OpCode::BCS, 2, 2, AddressingMode::NoneAddressing)),
            (0x90, OpCodeInfo::new(OpCode::BCC, 2, 2, AddressingMode::NoneAddressing)),

            (0x10, OpCodeInfo::new(OpCode::BPL, 2, 2, AddressingMode::NoneAddressing)),
            (0x30, OpCodeInfo::new(OpCode::BMI, 2, 2, AddressingMode::NoneAddressing)),

            (0xc6, OpCodeInfo::new(OpCode::DEC, 2, 5, AddressingMode::ZeroPage)),
            (0xd6, OpCodeInfo::new(OpCode::DEC, 2, 6, AddressingMode::ZeroPageX)),
            (0xce, OpCodeInfo::new(OpCode::DEC, 3, 6, AddressingMode::Absolute)),
            (0xde, OpCodeInfo::new(OpCode::DEC, 3, 7, AddressingMode::AbsoluteX)),

            (0xca, OpCodeInfo::new(OpCode::DEX, 1, 2, AddressingMode::NoneAddressing)),
            (0x88, OpCodeInfo::new(OpCode::DEY, 1, 2, AddressingMode::NoneAddressing)),

            (0x4a, OpCodeInfo::new(OpCode::LSR, 1, 2, AddressingMode::NoneAddressing)),
            (0x46, OpCodeInfo::new(OpCode::LSR, 2, 5, AddressingMode::ZeroPage)),
            (0x56, OpCodeInfo::new(OpCode::LSR, 2, 6, AddressingMode::ZeroPageX)),
            (0x4e, OpCodeInfo::new(OpCode::LSR, 3, 6, AddressingMode::Absolute)),
            (0x5e, OpCodeInfo::new(OpCode::LSR, 3, 7, AddressingMode::AbsoluteX)),

            (0x0a, OpCodeInfo::new(OpCode::ASL, 1, 2, AddressingMode::NoneAddressing)),
            (0x06, OpCodeInfo::new(OpCode::ASL, 2, 5, AddressingMode::ZeroPage)),
            (0x16, OpCodeInfo::new(OpCode::ASL, 2, 6, AddressingMode::ZeroPageX)),
            (0x0e, OpCodeInfo::new(OpCode::ASL, 3, 6, AddressingMode::Absolute)),
            (0x1e, OpCodeInfo::new(OpCode::ASL, 3, 7, AddressingMode::AbsoluteX)),

            (0x2a, OpCodeInfo::new(OpCode::ROL, 1, 2, AddressingMode::NoneAddressing)),
            (0x26, OpCodeInfo::new(OpCode::ROL, 2, 5, AddressingMode::ZeroPage)),
            (0x36, OpCodeInfo::new(OpCode::ROL, 2, 6, AddressingMode::ZeroPageX)),
            (0x2e, OpCodeInfo::new(OpCode::ROL, 3, 6, AddressingMode::Absolute)),
            (0x3e, OpCodeInfo::new(OpCode::ROL, 3, 7, AddressingMode::AbsoluteX)),

            (0x6a, OpCodeInfo::new(OpCode::ROR, 1, 2, AddressingMode::NoneAddressing)),
            (0x66, OpCodeInfo::new(OpCode::ROR, 2, 5, AddressingMode::ZeroPage)),
            (0x76, OpCodeInfo::new(OpCode::ROR, 2, 6, AddressingMode::ZeroPageX)),
            (0x6e, OpCodeInfo::new(OpCode::ROR, 3, 6, AddressingMode::Absolute)),
            (0x7e, OpCodeInfo::new(OpCode::ROR, 3, 7, AddressingMode::AbsoluteX)),

            (0x24, OpCodeInfo::new(OpCode::BIT, 2, 3, AddressingMode::ZeroPage)),
            (0x2c, OpCodeInfo::new(OpCode::BIT, 3, 4, AddressingMode::Absolute)),

            (0x4c, OpCodeInfo::new(OpCode::JMP, 3, 3, AddressingMode::Absolute)),
            (0x6c, OpCodeInfo::new(OpCode::JMP, 3, 5, AddressingMode::Indirect)),

            (0x48, OpCodeInfo::new(OpCode::PHA, 1, 3, AddressingMode::NoneAddressing)),
            (0x68, OpCodeInfo::new(OpCode::PLA, 1, 4, AddressingMode::NoneAddressing)),

            (0x50, OpCodeInfo::new(OpCode::BVC, 2, 2, AddressingMode::NoneAddressing)),
            (0x70, OpCodeInfo::new(OpCode::BVS, 2, 2, AddressingMode::NoneAddressing)),

            (0x08, OpCodeInfo::new(OpCode::PHP, 1, 3, AddressingMode::NoneAddressing)),
            (0x28, OpCodeInfo::new(OpCode::PLP, 1, 4, AddressingMode::NoneAddressing)),
        ])
    };
}

#[derive(Debug)]
pub enum OpCode {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    TAX,
    TXA,
}

#[allow(dead_code)]
pub struct OpCodeInfo {
    opcode: OpCode,
    bytes: u8,
    cycles: u8,
    addressing_mode: AddressingMode,
}

impl OpCodeInfo {
    fn new(instruction: OpCode, bytes: u8, cycles: u8, addressing_mode: AddressingMode) -> Self {
        Self {
            opcode: instruction,
            bytes,
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

bitflags! {
    struct StatusFlags: u8 {
        const CARRY             = 1 << 0;
        const ZERO              = 1 << 1;
        const INTERRUPT_DISABLE = 1 << 2;
        const DECIMAL_MODE      = 1 << 3;
        // No CPU effect
        const BREAK_COMMAND     = 1 << 4;
        // No CPU effect; always pushed as 1
        const UNUSED            = 1 << 5;
        const OVERFLOW          = 1 << 6;
        const NEGATIVE          = 1 << 7;
    }
}

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: StatusFlags,
    stack_pointer: u8,
    program_counter: u16,
    memory: [u8; u16::MAX as usize],
}

impl CPU {
    pub fn new() -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: StatusFlags::from_bits_truncate(0b0010_0000),
            stack_pointer: 0xff,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn run_tests(&mut self, program: Vec<u8>) {
        self.load(program);
        self.program_counter = self.read_u16(0xFFFC);
        self.run();
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = StatusFlags::empty();
        self.program_counter = self.read_u16(0xFFFC);
    }

    fn read_u8(&mut self, addr: u16) -> u8 {
        self.program_counter += 1;
        self.memory[addr as usize]
    }

    fn read_i8(&mut self, addr: u16) -> i8 {
        self.read_u8(addr) as i8
    }

    fn read_u16(&mut self, addr: u16) -> u16 {
        u16::from_le_bytes([self.read_u8(addr), self.read_u8(addr + 1)])
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let [lo, hi] = u16::to_le_bytes(data);
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    fn stack_push_u8(&mut self, data: u8) {
        self.memory[self.stack_pointer as usize + STACK_BASE as usize] = data;
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u8(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.memory[self.stack_pointer as usize + STACK_BASE as usize]
    }

    fn stack_push_u16(&mut self, data: u16) {
        let [lo, hi] = u16::to_le_bytes(data);
        self.stack_push_u8(hi);
        self.stack_push_u8(lo);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        u16::from_le_bytes([self.stack_pop_u8(), self.stack_pop_u8()])
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn load(&mut self, program: Vec<u8>) {
        let start_addr = 0x0600 /*0x8000*/;
        self.memory[start_addr..start_addr + program.len()].copy_from_slice(&program);
        self.mem_write_u16(0xFFFC, start_addr as u16);
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
                let value = self.memory[addr as usize];
                self.status
                    .set(StatusFlags::CARRY, value & 0b0000_0001 == 1);
                let result = value.wrapping_shr(1);
                self.mem_write(addr, result);
                result
            }
        };

        self.update_zero_and_negative_flags(result);
    }

    fn branch(&mut self, condition: bool) {
        let value = self.read_i8(self.program_counter);
        if condition {
            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
        }
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
            AddressingMode::ZeroPage => self.read_u8(self.program_counter).into(),
            AddressingMode::ZeroPageX => {
                let pos = self.read_u8(self.program_counter);
                pos.wrapping_add(self.register_x).into()
            }
            AddressingMode::ZeroPageY => {
                let pos = self.read_u8(self.program_counter);
                pos.wrapping_add(self.register_y).into()
            }
            AddressingMode::Absolute => self.read_u16(self.program_counter),
            AddressingMode::AbsoluteX => {
                let base = self.read_u16(self.program_counter);
                base.wrapping_add(self.register_x.into())
            }
            AddressingMode::AbsoluteY => {
                let base = self.read_u16(self.program_counter);
                base.wrapping_add(self.register_y.into())
            }
            AddressingMode::Indirect => {
                todo!()
            }
            AddressingMode::IndirectX => {
                let base = self.read_u8(self.program_counter);

                let ptr = base.wrapping_add(self.register_x);
                let lo: u16 = self.memory[ptr as usize].into();
                let hi: u16 = self.memory[ptr.wrapping_add(1) as usize].into();
                hi << 8 | lo
            }
            AddressingMode::IndirectY => {
                let base = self.read_u8(self.program_counter);

                let lo: u16 = self.memory[base as usize].into();
                let hi: u16 = self.memory[base.wrapping_add(1) as usize].into();
                let deref_base = hi << 8 | lo;
                deref_base.wrapping_add(self.register_y.into())
            }
            AddressingMode::NoneAddressing => panic!("mode {:?} is not supported", mode),
        }
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
        where
            F: FnMut(&mut CPU),
    {
        loop {
            callback(self);

            let opcode = self.read_u8(self.program_counter);
            let opcode = match OP_CODES_MAP.get(&opcode) {
                Some(opcode) => opcode,
                None => todo!("{}", format!("opcode: {:#02x?}", opcode)),
            };

            match opcode.opcode {
                OpCode::PHP => self.stack_push_u8(self.status.bits()),
                OpCode::PLP => self.status = StatusFlags::from_bits_truncate(self.stack_pop_u8()),
                OpCode::PHA => {
                    self.mem_write(self.stack_pointer as u16 + STACK_BASE, self.register_a);
                    self.stack_pointer = self.stack_pointer.wrapping_sub(1);
                }
                OpCode::PLA => {
                    self.register_a =
                        self.memory[self.stack_pointer as usize + STACK_BASE as usize];
                    self.stack_pointer = self.stack_pointer.wrapping_add(1);
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::LDA => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_a = self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::LDX => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_x = self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_x);
                }
                OpCode::LDY => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_y = self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_y);
                }
                OpCode::STA => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.mem_write(addr, self.register_a);
                }

                OpCode::JSR => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.stack_push_u16(self.program_counter);
                    self.program_counter = addr;
                }
                OpCode::RTS => self.program_counter = self.stack_pop_u16(),

                OpCode::AND => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_a = self.register_a & self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::ORA => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_a = self.register_a | self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::EOR => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    self.register_a = self.register_a ^ self.memory[addr as usize];
                    self.update_zero_and_negative_flags(self.register_a);
                }

                // TODO: cleanup & handle overflow bit
                OpCode::ADC => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let (value, overflow1) =
                        self.register_a.overflowing_add(self.memory[addr as usize]);
                    let (result, overflow2) =
                        value.overflowing_add(self.status.contains(StatusFlags::CARRY) as u8);
                    self.register_a = result;
                    self.status.set(StatusFlags::CARRY, overflow1 || overflow2);
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::SBC => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let (value, overflow1) =
                        self.register_a.overflowing_sub(self.memory[addr as usize]);
                    let (result, overflow2) =
                        value.overflowing_sub(!self.status.contains(StatusFlags::CARRY) as u8);
                    if overflow1 || overflow2 {
                        self.status.remove(StatusFlags::CARRY);
                    }
                    self.register_a = result;
                    self.update_zero_and_negative_flags(self.register_a);
                }

                OpCode::CMP => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize];
                    let result = self.register_a.wrapping_sub(value);
                    if self.register_a >= value {
                        self.status.insert(StatusFlags::CARRY);
                    }

                    self.update_zero_and_negative_flags(result);
                }
                OpCode::CPX => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize];
                    let result = self.register_x.wrapping_sub(value);
                    if self.register_x >= value {
                        self.status.insert(StatusFlags::CARRY);
                    }

                    self.update_zero_and_negative_flags(result);
                }
                OpCode::CPY => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize];
                    let result = self.register_y.wrapping_sub(value);
                    if self.register_y >= self.memory[addr as usize] {
                        self.status.insert(StatusFlags::CARRY);
                    }

                    self.update_zero_and_negative_flags(result);
                }
                OpCode::BIT => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize];
                    self.status
                        .set(StatusFlags::OVERFLOW, value & 0b0100_0000 == 1);
                    self.status
                        .set(StatusFlags::NEGATIVE, value & 0b1000_0000 == 1);
                    if value & self.register_a == 0 {
                        self.status.insert(StatusFlags::ZERO);
                    }
                }

                OpCode::JMP => self.program_counter = self.operand_address(&opcode.addressing_mode),
                OpCode::BEQ => self.branch(self.status.contains(StatusFlags::ZERO)),
                OpCode::BNE => self.branch(!self.status.contains(StatusFlags::ZERO)),
                OpCode::BPL => self.branch(!self.status.contains(StatusFlags::NEGATIVE)),
                OpCode::BMI => self.branch(self.status.contains(StatusFlags::NEGATIVE)),
                OpCode::BCS => self.branch(self.status.contains(StatusFlags::CARRY)),
                OpCode::BCC => self.branch(!self.status.contains(StatusFlags::CARRY)),
                OpCode::BVC => self.branch(!self.status.contains(StatusFlags::OVERFLOW)),
                OpCode::BVS => self.branch(self.status.contains(StatusFlags::OVERFLOW)),

                OpCode::DEC => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize].wrapping_sub(1);
                    self.mem_write(addr, value);
                    self.update_zero_and_negative_flags(value);
                }
                OpCode::DEX => {
                    self.register_x = self.register_x.wrapping_sub(1);
                    self.update_zero_and_negative_flags(self.register_x);
                }
                OpCode::DEY => {
                    self.register_y = self.register_y.wrapping_sub(1);
                    self.update_zero_and_negative_flags(self.register_y);
                }
                OpCode::TAX => {
                    self.register_x = self.register_a;
                    self.update_zero_and_negative_flags(self.register_x);
                }
                OpCode::TXA => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                }
                OpCode::INC => {
                    let addr = self.operand_address(&opcode.addressing_mode);
                    let value = self.memory[addr as usize].wrapping_add(1);
                    self.mem_write(addr, value);
                    self.update_zero_and_negative_flags(value);
                }
                OpCode::INX => {
                    self.register_x = self.register_x.wrapping_add(1);
                    self.update_zero_and_negative_flags(self.register_x);
                }
                OpCode::INY => {
                    self.register_y = self.register_y.wrapping_add(1);
                    self.update_zero_and_negative_flags(self.register_y);
                }
                OpCode::LSR => self.lsr(&opcode.addressing_mode),
                OpCode::ROL => {}
                OpCode::ROR => {}
                OpCode::ASL => {}
                OpCode::SEC => self.status.insert(StatusFlags::CARRY),
                OpCode::CLC => self.status.remove(StatusFlags::CARRY),
                OpCode::SED => self.status.insert(StatusFlags::DECIMAL_MODE),
                OpCode::CLD => self.status.remove(StatusFlags::DECIMAL_MODE),
                OpCode::SEI => self.status.insert(StatusFlags::INTERRUPT_DISABLE),
                OpCode::CLI => self.status.remove(StatusFlags::INTERRUPT_DISABLE),
                OpCode::CLV => self.status.remove(StatusFlags::OVERFLOW),
                OpCode::NOP => {}
                OpCode::BRK => {
                    self.status.insert(StatusFlags::BREAK_COMMAND);
                    return
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(StatusFlags::ZERO));
        assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0x00, 0x00]);

        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.run_tests(vec![0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.run_tests(vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_subroutines_with_inx() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x60,
        ]);

        assert_eq!(cpu.register_x, 3);
    }

    #[test]
    fn test_nested_subroutines() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x20, 0x0f, 0x06, 0x60,
            0x00, 0xe8, 0x60, 0x00,
        ]);

        assert_eq!(cpu.register_x, 5);
    }

    #[test]
    fn test_and() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0x10, 0x29, 0x30]);

        assert_eq!(cpu.register_a, 0x10);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_and_zero() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0x1, 0x29, 0x2]);

        assert_eq!(cpu.register_a, 0);
        assert!(!cpu.status.is_empty());
    }

    #[test]
    fn test_adc() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0xff, 0x69, 0x1, 0x69, 0x1]);

        assert_eq!(cpu.register_a, 2);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_dex_bpl() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa2, 0x4, 0xca, 0xca, 0x10, 0xfd]);

        assert_eq!(cpu.register_x, 0xff);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => cpu.mem_write(0xff, 0x77),
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => cpu.mem_write(0xff, 0x61),
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => cpu.mem_write(0xff, 0x73),
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => cpu.mem_write(0xff, 0x64),
            _ => {}
        }
    }
}

fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200..0x600 {
        let (b1, b2, b3) = color(cpu.memory[i]).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn main() {
    let game_code = vec![
        0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9, 0x02,
        0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85, 0x12, 0xa9,
        0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60, 0xa5, 0xfe, 0x85,
        0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60, 0x20, 0x4d, 0x06, 0x20,
        0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20, 0x07, 0x20, 0x2d, 0x07, 0x4c,
        0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9, 0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0,
        0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04, 0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85,
        0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0, 0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01,
        0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04, 0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05,
        0xa9, 0x08, 0x85, 0x02, 0x60, 0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00,
        0xc5, 0x10, 0xd0, 0x0d, 0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20,
        0x2a, 0x06, 0x60, 0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11,
        0xf0, 0x09, 0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60,
        0xa6, 0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
        0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9, 0x20,
        0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28, 0x60, 0xe6,
        0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69, 0x20, 0x85, 0x10,
        0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c, 0x60, 0xc6, 0x10, 0xa5,
        0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35, 0x07, 0xa0, 0x00, 0xa5, 0xfe,
        0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10, 0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10,
        0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb, 0x60,
    ];

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Snake6502", (32.0 * 20.0) as u32, (32.0 * 20.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(20.0, 20.0).unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    let mut cpu = CPU::new();
    cpu.load(game_code);
    cpu.reset();

    let mut screen_state = [0u8; 32 * 32 * 3];
    let mut rng = thread_rng();

    cpu.run_with_callback(move |cpu| {
        handle_user_input(cpu, &mut event_pump);
        cpu.mem_write(0xfe, rng.gen_range(1..16));
        if read_screen_state(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        std::thread::sleep(std::time::Duration::new(0, 70_000));
    });
}
