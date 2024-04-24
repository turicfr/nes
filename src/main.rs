use std::collections::HashMap;
use bitflags::bitflags;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref CPU_OP_CODES_MAP: HashMap<u8, OpCodeInfo> = {
        HashMap::from([
            (0x00, OpCodeInfo::new(OpCode::BRK, 1, 7, AddressingMode::NoneAddressing)),
            (0xaa, OpCodeInfo::new(OpCode::TAX, 1, 2, AddressingMode::NoneAddressing)),
            (0xe8, OpCodeInfo::new(OpCode::INX, 1, 2, AddressingMode::NoneAddressing)),

            (0xa9, OpCodeInfo::new(OpCode::LDA, 2, 2, AddressingMode::Immediate)),
            (0xa5, OpCodeInfo::new(OpCode::LDA, 2, 3, AddressingMode::ZeroPage)),
            (0xb5, OpCodeInfo::new(OpCode::LDA, 2, 4, AddressingMode::ZeroPage_X)),
            (0xad, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::Absolute)),
            (0xbd, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::Absolute_X)),
            (0xb9, OpCodeInfo::new(OpCode::LDA, 3, 4, AddressingMode::Absolute_Y)),
            (0xa1, OpCodeInfo::new(OpCode::LDA, 2, 6, AddressingMode::Indirect_X)),
            (0xb1, OpCodeInfo::new(OpCode::LDA, 2, 5, AddressingMode::Indirect_Y)),

            (0x85, OpCodeInfo::new(OpCode::STA, 2, 3, AddressingMode::ZeroPage)),
            (0x95, OpCodeInfo::new(OpCode::STA, 2, 4, AddressingMode::ZeroPage_X)),
            (0x8d, OpCodeInfo::new(OpCode::STA, 3, 4, AddressingMode::Absolute)),
            (0x9d, OpCodeInfo::new(OpCode::STA, 3, 5, AddressingMode::Absolute_X)),
            (0x99, OpCodeInfo::new(OpCode::STA, 3, 5, AddressingMode::Absolute_Y)),
            (0x81, OpCodeInfo::new(OpCode::STA, 2, 6, AddressingMode::Indirect_X)),
            (0x91, OpCodeInfo::new(OpCode::STA, 2, 6, AddressingMode::Indirect_Y)),
        ])
    };
}

pub enum OpCode {
    LDA,
    STA,
    BRK,
    TAX,
    INX,
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
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

bitflags! {
    /// Represents a set of flags.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Status: u8 {
        const CARRY             = 0b0000_0001;
        const ZERO              = 0b0000_0010;
        const DISABLE_INTERRUPT = 0b0000_0100;
        const DECIMAL_MODE      = 0b0000_1000;
        const BREAK_COMMAND     = 0b0001_0000;
        const OVERFLOW          = 0b0010_0000;
        const NEGATIVE          = 0b1000_0000;
    }
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    status: Status,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: Status::empty(),
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
        self.status = Status::empty();
        self.program_counter = self.read_u16(0xFFFC);
    }

    fn read_u8(&mut self, addr: u16) -> u8 {
        self.program_counter += 1;
        self.memory[addr as usize]
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

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.read_u8(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        // Update zero flag.
        if result == 0 {
            self.status = self.status | Status::ZERO;
        } else {
            self.status = self.status - Status::ZERO;
        }

        // Update negative flag.
        if result & 0b1000_0000 != 0 {
            self.status = self.status | Status::NEGATIVE;
        } else {
            self.status = self.status - Status::NEGATIVE;
        }
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.read_u8(self.program_counter) as u16,
            AddressingMode::ZeroPage_X => {
                let pos = self.read_u8(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.read_u8(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute => self.read_u16(self.program_counter),
            AddressingMode::Absolute_X => {
                let base = self.read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect_X => {
                let base = self.read_u8(self.program_counter);

                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.read_u8(ptr as u16);
                let hi = self.read_u8(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.read_u8(self.program_counter);

                let lo = self.read_u8(base as u16);
                let hi = self.read_u8(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    pub fn run(&mut self) {
        loop {
            let opcode = self.read_u8(self.program_counter);
            let opcode = match CPU_OP_CODES_MAP.get(&opcode) {
                Some(opcode) => opcode,
                None => todo!(),
            };

            match opcode.opcode {
                OpCode::LDA => self.lda(&opcode.addressing_mode),
                OpCode::STA => self.sta(&opcode.addressing_mode),
                OpCode::TAX => self.tax(),
                OpCode::INX => self.inx(),
                OpCode::BRK => return,
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
        assert_eq!((cpu.status & Status::ZERO).bits(), 0);
        assert_eq!((cpu.status & Status::NEGATIVE).bits(), 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.run_tests(vec![0xa9, 0x00, 0x00]);
        assert_eq!((cpu.status & Status::ZERO).bits(), 0b10);
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
}

fn main() {
    println!("Hello, world!");
}
