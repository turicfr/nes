use crate::bus::Bus;
use crate::carrying::CarryingExt;
use bitflags::bitflags;
use lazy_static::lazy_static;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::ops::Shl;

const STACK_BASE: u16 = 0x100;

type OpcodeMethod = fn(&mut CPU, Option<u16>);

lazy_static! {
    // References:
    // * https://www.nesdev.org/obelisk-6502-guide/reference.html
    // * https://web.archive.org/web/20220831224234if_/https://users.telenet.be/kim1-6502/6502/proman.html
    pub static ref OP_CODES_MAP: HashMap<u8, Instruction> = {
        HashMap::from([
            (0x00, Instruction::new(CPU::brk, "BRK", 7, AddressingMode::None)),
            (0xea, Instruction::new(CPU::nop, "NOP", 2, AddressingMode::None)),
            (0x18, Instruction::new(CPU::clc, "CLC", 2, AddressingMode::None)),
            (0x38, Instruction::new(CPU::sec, "SEC", 2, AddressingMode::None)),
            (0xd8, Instruction::new(CPU::cld, "CLD", 2, AddressingMode::None)),
            (0xf8, Instruction::new(CPU::sed, "SED", 2, AddressingMode::None)),
            (0xb8, Instruction::new(CPU::clv, "CLV", 2, AddressingMode::None)),
            (0x58, Instruction::new(CPU::cli, "CLI", 2, AddressingMode::None)),
            (0x78, Instruction::new(CPU::sei, "SEI", 2, AddressingMode::None)),
            (0xaa, Instruction::new(CPU::tax, "TAX", 2, AddressingMode::None)),
            (0x8a, Instruction::new(CPU::txa, "TXA", 2, AddressingMode::None)),
            (0x9a, Instruction::new(CPU::txs, "TXS", 2, AddressingMode::None)),
            (0xba, Instruction::new(CPU::tsx, "TSX", 2, AddressingMode::None)),

            (0x20, Instruction::new(CPU::jsr, "JSR", 6, AddressingMode::Absolute)),
            (0xe8, Instruction::new(CPU::inx, "INX", 2, AddressingMode::None)),
            (0xc8, Instruction::new(CPU::iny, "INY", 2, AddressingMode::None)),
            (0x60, Instruction::new(CPU::rts, "RTS", 6, AddressingMode::None)),

            (0xe6, Instruction::new(CPU::inc, "INC", 5, AddressingMode::ZeroPage)),
            (0xf6, Instruction::new(CPU::inc, "INC", 6, AddressingMode::ZeroPageX)),
            (0xee, Instruction::new(CPU::inc, "INC", 6, AddressingMode::Absolute)),
            (0xfe, Instruction::new(CPU::inc, "INC", 7, AddressingMode::AbsoluteX)),

            (0xa9, Instruction::new(CPU::lda, "LDA", 2, AddressingMode::Immediate)),
            (0xa5, Instruction::new(CPU::lda, "LDA", 3, AddressingMode::ZeroPage)),
            (0xb5, Instruction::new(CPU::lda, "LDA", 4, AddressingMode::ZeroPageX)),
            (0xad, Instruction::new(CPU::lda, "LDA", 4, AddressingMode::Absolute)),
            (0xbd, Instruction::new(CPU::lda, "LDA", 4, AddressingMode::AbsoluteX)),
            (0xb9, Instruction::new(CPU::lda, "LDA", 4, AddressingMode::AbsoluteY)),
            (0xa1, Instruction::new(CPU::lda, "LDA", 6, AddressingMode::IndirectX)),
            (0xb1, Instruction::new(CPU::lda, "LDA", 5, AddressingMode::IndirectY)),

            (0xa2, Instruction::new(CPU::ldx, "LDX", 2, AddressingMode::Immediate)),
            (0xa6, Instruction::new(CPU::ldx, "LDX", 3, AddressingMode::ZeroPage)),
            (0xb6, Instruction::new(CPU::ldx, "LDX", 4, AddressingMode::ZeroPageY)),
            (0xae, Instruction::new(CPU::ldx, "LDX", 4, AddressingMode::Absolute)),
            (0xbe, Instruction::new(CPU::ldx, "LDX", 4, AddressingMode::AbsoluteY)),

            (0xa0, Instruction::new(CPU::ldy, "LDY", 2, AddressingMode::Immediate)),
            (0xa4, Instruction::new(CPU::ldy, "LDY", 3, AddressingMode::ZeroPage)),
            (0xb4, Instruction::new(CPU::ldy, "LDY", 4, AddressingMode::ZeroPageX)),
            (0xac, Instruction::new(CPU::ldy, "LDY", 4, AddressingMode::Absolute)),
            (0xbc, Instruction::new(CPU::ldy, "LDY", 4, AddressingMode::AbsoluteX)),

            (0x85, Instruction::new(CPU::sta, "STA", 3, AddressingMode::ZeroPage)),
            (0x95, Instruction::new(CPU::sta, "STA", 4, AddressingMode::ZeroPageX)),
            (0x8d, Instruction::new(CPU::sta, "STA", 4, AddressingMode::Absolute)),
            (0x9d, Instruction::new(CPU::sta, "STA", 5, AddressingMode::AbsoluteX)),
            (0x99, Instruction::new(CPU::sta, "STA", 5, AddressingMode::AbsoluteY)),
            (0x81, Instruction::new(CPU::sta, "STA", 6, AddressingMode::IndirectX)),
            (0x91, Instruction::new(CPU::sta, "STA", 6, AddressingMode::IndirectY)),

            (0x86, Instruction::new(CPU::stx, "STX", 3, AddressingMode::ZeroPage)),
            (0x96, Instruction::new(CPU::stx, "STX", 4, AddressingMode::ZeroPageY)),
            (0x8e, Instruction::new(CPU::stx, "STX", 4, AddressingMode::Absolute)),

            (0x84, Instruction::new(CPU::sty, "STY", 3, AddressingMode::ZeroPage)),
            (0x94, Instruction::new(CPU::sty, "STY", 4, AddressingMode::ZeroPageX)),
            (0x8c, Instruction::new(CPU::sty, "STY", 4, AddressingMode::Absolute)),

            (0x29, Instruction::new(CPU::and, "AND", 2, AddressingMode::Immediate)),
            (0x25, Instruction::new(CPU::and, "AND", 3, AddressingMode::ZeroPage)),
            (0x35, Instruction::new(CPU::and, "AND", 4, AddressingMode::ZeroPageX)),
            (0x2D, Instruction::new(CPU::and, "AND", 4, AddressingMode::Absolute)),
            (0x3D, Instruction::new(CPU::and, "AND", 4, AddressingMode::AbsoluteX)),
            (0x39, Instruction::new(CPU::and, "AND", 4, AddressingMode::AbsoluteY)),
            (0x21, Instruction::new(CPU::and, "AND", 6, AddressingMode::IndirectX)),
            (0x31, Instruction::new(CPU::and, "AND", 5, AddressingMode::IndirectY)),

            (0x49, Instruction::new(CPU::eor, "EOR", 2, AddressingMode::Immediate)),
            (0x45, Instruction::new(CPU::eor, "EOR", 3, AddressingMode::ZeroPage)),
            (0x55, Instruction::new(CPU::eor, "EOR", 4, AddressingMode::ZeroPageX)),
            (0x4D, Instruction::new(CPU::eor, "EOR", 4, AddressingMode::Absolute)),
            (0x5D, Instruction::new(CPU::eor, "EOR", 4, AddressingMode::AbsoluteX)),
            (0x59, Instruction::new(CPU::eor, "EOR", 4, AddressingMode::AbsoluteY)),
            (0x41, Instruction::new(CPU::eor, "EOR", 6, AddressingMode::IndirectX)),
            (0x51, Instruction::new(CPU::eor, "EOR", 5, AddressingMode::IndirectY)),

            (0x09, Instruction::new(CPU::ora, "ORA", 2, AddressingMode::Immediate)),
            (0x05, Instruction::new(CPU::ora, "ORA", 3, AddressingMode::ZeroPage)),
            (0x15, Instruction::new(CPU::ora, "ORA", 4, AddressingMode::ZeroPageX)),
            (0x0d, Instruction::new(CPU::ora, "ORA", 4, AddressingMode::Absolute)),
            (0x1d, Instruction::new(CPU::ora, "ORA", 4, AddressingMode::AbsoluteX)),
            (0x19, Instruction::new(CPU::ora, "ORA", 4, AddressingMode::AbsoluteY)),
            (0x01, Instruction::new(CPU::ora, "ORA", 6, AddressingMode::IndirectX)),
            (0x11, Instruction::new(CPU::ora, "ORA", 5, AddressingMode::IndirectY)),

            (0x69, Instruction::new(CPU::adc, "ADC", 2, AddressingMode::Immediate)),
            (0x65, Instruction::new(CPU::adc, "ADC", 3, AddressingMode::ZeroPage)),
            (0x75, Instruction::new(CPU::adc, "ADC", 4, AddressingMode::ZeroPageX)),
            (0x6d, Instruction::new(CPU::adc, "ADC", 4, AddressingMode::Absolute)),
            (0x7d, Instruction::new(CPU::adc, "ADC", 4, AddressingMode::AbsoluteX)),
            (0x79, Instruction::new(CPU::adc, "ADC", 4, AddressingMode::AbsoluteY)),
            (0x61, Instruction::new(CPU::adc, "ADC", 6, AddressingMode::IndirectX)),
            (0x71, Instruction::new(CPU::adc, "ADC", 5, AddressingMode::IndirectY)),

            (0xe9, Instruction::new(CPU::sbc, "SBC", 2, AddressingMode::Immediate)),
            (0xe5, Instruction::new(CPU::sbc, "SBC", 3, AddressingMode::ZeroPage)),
            (0xf5, Instruction::new(CPU::sbc, "SBC", 4, AddressingMode::ZeroPageX)),
            (0xed, Instruction::new(CPU::sbc, "SBC", 4, AddressingMode::Absolute)),
            (0xfd, Instruction::new(CPU::sbc, "SBC", 4, AddressingMode::AbsoluteX)),
            (0xf9, Instruction::new(CPU::sbc, "SBC", 4, AddressingMode::AbsoluteY)),
            (0xe1, Instruction::new(CPU::sbc, "SBC", 6, AddressingMode::IndirectX)),
            (0xf1, Instruction::new(CPU::sbc, "SBC", 5, AddressingMode::IndirectY)),

            (0xc9, Instruction::new(CPU::cmp, "CMP", 2, AddressingMode::Immediate)),
            (0xc5, Instruction::new(CPU::cmp, "CMP", 3, AddressingMode::ZeroPage)),
            (0xd5, Instruction::new(CPU::cmp, "CMP", 4, AddressingMode::ZeroPageX)),
            (0xcd, Instruction::new(CPU::cmp, "CMP", 4, AddressingMode::Absolute)),
            (0xdd, Instruction::new(CPU::cmp, "CMP", 4, AddressingMode::AbsoluteX)),
            (0xd9, Instruction::new(CPU::cmp, "CMP", 4, AddressingMode::AbsoluteY)),
            (0xc1, Instruction::new(CPU::cmp, "CMP", 6, AddressingMode::IndirectX)),
            (0xd1, Instruction::new(CPU::cmp, "CMP", 5, AddressingMode::IndirectY)),

            (0xe0, Instruction::new(CPU::cpx, "CPX", 2, AddressingMode::Immediate)),
            (0xe4, Instruction::new(CPU::cpx, "CPX", 3, AddressingMode::ZeroPage)),
            (0xec, Instruction::new(CPU::cpx, "CPX", 4, AddressingMode::Absolute)),

            (0xc0, Instruction::new(CPU::cpy, "CPY", 2, AddressingMode::Immediate)),
            (0xc4, Instruction::new(CPU::cpy, "CPY", 3, AddressingMode::ZeroPage)),
            (0xcc, Instruction::new(CPU::cpy, "CPY", 4, AddressingMode::Absolute)),

            (0xf0, Instruction::new(CPU::beq, "BEQ", 2, AddressingMode::Relative)),
            (0xd0, Instruction::new(CPU::bne, "BNE", 2, AddressingMode::Relative)),
            (0xb0, Instruction::new(CPU::bcs, "BCS", 2, AddressingMode::Relative)),
            (0x90, Instruction::new(CPU::bcc, "BCC", 2, AddressingMode::Relative)),
            (0x10, Instruction::new(CPU::bpl, "BPL", 2, AddressingMode::Relative)),
            (0x30, Instruction::new(CPU::bmi, "BMI", 2, AddressingMode::Relative)),
            (0x50, Instruction::new(CPU::bvc, "BVC", 2, AddressingMode::Relative)),
            (0x70, Instruction::new(CPU::bvs, "BVS", 2, AddressingMode::Relative)),

            (0xc6, Instruction::new(CPU::dec, "DEC", 5, AddressingMode::ZeroPage)),
            (0xd6, Instruction::new(CPU::dec, "DEC", 6, AddressingMode::ZeroPageX)),
            (0xce, Instruction::new(CPU::dec, "DEC", 6, AddressingMode::Absolute)),
            (0xde, Instruction::new(CPU::dec, "DEC", 7, AddressingMode::AbsoluteX)),

            (0xca, Instruction::new(CPU::dex, "DEX", 2, AddressingMode::None)),
            (0x88, Instruction::new(CPU::dey, "DEY", 2, AddressingMode::None)),

            (0x4a, Instruction::new(CPU::lsr, "LSR", 2, AddressingMode::Accumulator)),
            (0x46, Instruction::new(CPU::lsr, "LSR", 5, AddressingMode::ZeroPage)),
            (0x56, Instruction::new(CPU::lsr, "LSR", 6, AddressingMode::ZeroPageX)),
            (0x4e, Instruction::new(CPU::lsr, "LSR", 6, AddressingMode::Absolute)),
            (0x5e, Instruction::new(CPU::lsr, "LSR", 7, AddressingMode::AbsoluteX)),

            (0x24, Instruction::new(CPU::bit, "BIT", 3, AddressingMode::ZeroPage)),
            (0x2c, Instruction::new(CPU::bit, "BIT", 4, AddressingMode::Absolute)),

            (0x4c, Instruction::new(CPU::jmp, "JMP", 3, AddressingMode::Absolute)),
            (0x6c, Instruction::new(CPU::jmp, "JMP", 5, AddressingMode::Indirect)),

            (0x48, Instruction::new(CPU::pha, "PHA", 3, AddressingMode::None)),
            (0x68, Instruction::new(CPU::pla, "PLA", 4, AddressingMode::None)),

            (0x08, Instruction::new(CPU::php, "PHP", 3, AddressingMode::None)),
            (0x28, Instruction::new(CPU::plp, "PLP", 4, AddressingMode::None)),

            (0xa8, Instruction::new(CPU::tay, "TAY", 2, AddressingMode::None)),
            (0x98, Instruction::new(CPU::tya, "TYA", 2, AddressingMode::None)),

            (0x40, Instruction::new(CPU::rti, "RTI", 6, AddressingMode::None)),

            (0x0a, Instruction::new(CPU::asl, "ASL", 2, AddressingMode::Accumulator)),
            (0x06, Instruction::new(CPU::asl, "ASL", 5, AddressingMode::ZeroPage)),
            (0x16, Instruction::new(CPU::asl, "ASL", 6, AddressingMode::ZeroPageX)),
            (0x0e, Instruction::new(CPU::asl, "ASL", 6, AddressingMode::Absolute)),
            (0x1e, Instruction::new(CPU::asl, "ASL", 7, AddressingMode::AbsoluteX)),

            (0x2a, Instruction::new(CPU::rol, "ROL", 2, AddressingMode::Accumulator)),
            (0x26, Instruction::new(CPU::rol, "ROL", 5, AddressingMode::ZeroPage)),
            (0x36, Instruction::new(CPU::rol, "ROL", 6, AddressingMode::ZeroPageX)),
            (0x2e, Instruction::new(CPU::rol, "ROL", 6, AddressingMode::Absolute)),
            (0x3e, Instruction::new(CPU::rol, "ROL", 7, AddressingMode::AbsoluteX)),

            (0x6a, Instruction::new(CPU::ror, "ROR", 2, AddressingMode::Accumulator)),
            (0x66, Instruction::new(CPU::ror, "ROR", 5, AddressingMode::ZeroPage)),
            (0x76, Instruction::new(CPU::ror, "ROR", 6, AddressingMode::ZeroPageX)),
            (0x6e, Instruction::new(CPU::ror, "ROR", 6, AddressingMode::Absolute)),
            (0x7e, Instruction::new(CPU::ror, "ROR", 7, AddressingMode::AbsoluteX)),
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
    name: &'static str,
    #[allow(dead_code)]
    cycles: u8,
    addressing_mode: AddressingMode,
}

impl Instruction {
    fn new(
        method: OpcodeMethod,
        name: &'static str,
        cycles: u8,
        addressing_mode: AddressingMode,
    ) -> Self {
        Self {
            method,
            name,
            cycles,
            addressing_mode,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Accumulator,
    None,
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

#[cfg(test)]
impl fmt::Debug for CPU {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;

        let opcode = self.peek_u8(self.program_counter);
        let instruction = match OP_CODES_MAP.get(&opcode) {
            Some(instruction) => instruction,
            None => todo!("opcode {opcode:02x} at {:04x}", self.program_counter),
        };
        let opcode = format!("{opcode:02X}");
        let name = instruction.name;
        let (bytes, instruction) = match &instruction.addressing_mode {
            AddressingMode::None => (opcode, format!("{name}")),
            AddressingMode::Accumulator => (opcode, format!("{name} A")),

            // 2 bytes
            AddressingMode::Relative => {
                let offset = self.peek_u8(self.program_counter + 1) as i8;
                let bytes = format!("{opcode} {offset:02X}");
                let target = (self.program_counter + 2).wrapping_add_signed(offset.into());
                let instruction = format!("{name} ${target:04X}");
                (bytes, instruction)
            }
            AddressingMode::Immediate => {
                let byte = self.peek_u8(self.program_counter + 1);
                let bytes = format!("{opcode} {byte:02X}");
                let instruction = format!("{name} #${byte:02X}");
                (bytes, instruction)
            }
            AddressingMode::ZeroPage => {
                let byte = self.peek_u8(self.program_counter + 1);
                let bytes = format!("{opcode} {byte:02X}");
                let instruction = format!("{name} ${byte:02X} = {:02X}", self.peek_u8(byte.into()));
                (bytes, instruction)
            }
            AddressingMode::ZeroPageX => {
                let byte = self.peek_u8(self.program_counter + 1);
                let addr = byte.wrapping_add(self.register_x);
                let value = self.peek_u8(addr.into());

                let bytes = format!("{opcode} {byte:02X}");
                let instruction = format!("{name} ${byte:02X},X @ {addr:02X} = {value:02X}");
                (bytes, instruction)
            }
            AddressingMode::ZeroPageY => {
                let byte = self.peek_u8(self.program_counter + 1);
                let addr = byte.wrapping_add(self.register_y);
                let value = self.peek_u8(addr.into());

                let bytes = format!("{opcode} {byte:02X}");
                let instruction = format!("{name} ${byte:02X},Y @ {addr:02X} = {value:02X}");
                (bytes, instruction)
            }
            AddressingMode::IndirectX => {
                let offset = self.peek_u8(self.program_counter + 1);
                let ptr = offset.wrapping_add(self.register_x);
                let target_addr = self.peek_u16_zero_page(ptr);
                let value = self.peek_u8(target_addr);

                let bytes = format!("{opcode} {offset:02X}");
                let instruction = format!(
                    "{name} (${offset:02X},X) @ {ptr:02X} = {target_addr:04X} = {value:02X}"
                );
                (bytes, instruction)
            }
            AddressingMode::IndirectY => {
                let byte = self.peek_u8(self.program_counter + 1);
                let base = self.peek_u16_zero_page(byte);
                let addr = base.wrapping_add(self.register_y.into());
                let value = self.peek_u8(addr);

                let bytes = format!("{opcode} {byte:02X}");
                let instruction =
                    format!("{name} (${byte:02X}),Y = {base:04X} @ {addr:04X} = {value:02X}");
                (bytes, instruction)
            }

            // 3 bytes
            AddressingMode::Absolute => {
                let lo = self.peek_u8(self.program_counter + 1);
                let hi = self.peek_u8(self.program_counter + 2);
                let bytes = format!("{opcode} {lo:02X} {hi:02X}");
                let addr = u16::from_le_bytes([lo, hi]);
                let mut instruction = format!("{name} ${addr:04X}");
                if !name.starts_with("J") {
                    let _ = write!(instruction, " = {:02X}", self.peek_u8(addr));
                }
                (bytes, instruction)
            }
            AddressingMode::AbsoluteX => {
                let lo = self.peek_u8(self.program_counter + 1);
                let hi = self.peek_u8(self.program_counter + 2);
                let ptr = u16::from_le_bytes([lo, hi]);
                let addr = ptr.wrapping_add(self.register_x.into());
                let value = self.peek_u8(addr);

                let bytes = format!("{opcode} {lo:02X} {hi:02X}");
                let instruction = format!("{name} ${ptr:04X},X @ {addr:04X} = {value:02X}");
                (bytes, instruction)
            }
            AddressingMode::AbsoluteY => {
                let lo = self.peek_u8(self.program_counter + 1);
                let hi = self.peek_u8(self.program_counter + 2);
                let ptr = u16::from_le_bytes([lo, hi]);
                let addr = ptr.wrapping_add(self.register_y.into());
                let value = self.peek_u8(addr);

                let bytes = format!("{opcode} {lo:02X} {hi:02X}");
                let instruction = format!("{name} ${ptr:04X},Y @ {addr:04X} = {value:02X}");
                (bytes, instruction)
            }
            AddressingMode::Indirect => {
                let lo = self.peek_u8(self.program_counter + 1);
                let hi = self.peek_u8(self.program_counter + 2);
                let ptr = u16::from_le_bytes([lo, hi]);
                let addr = u16::from_le_bytes([
                    self.peek_u8(ptr),
                    self.peek_u8(u16::from_le_bytes([lo.wrapping_add(1), hi])),
                ]);

                let bytes = format!("{opcode} {lo:02X} {hi:02X}");
                let instruction = format!("{name} (${ptr:04X}) = {addr:04X}");
                (bytes, instruction)
            }
        };

        write!(
            f,
            "{:04X}  {bytes:<8}  {instruction:<30}  A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.program_counter,
            self.register_a,
            self.register_x,
            self.register_y,
            self.status.bits(),
            self.stack_pointer,
        )
    }
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: StatusFlags::from_bits_truncate(0b0010_0100),
            stack_pointer: 0xfd,
            program_counter: 0,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = StatusFlags::from_bits_truncate(0b0010_0100);
        self.program_counter = self.peek_u16(0xfffc);
    }

    pub fn peek_u8(&self, addr: u16) -> u8 {
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

    fn peek_u16(&self, addr: u16) -> u16 {
        u16::from_le_bytes([self.peek_u8(addr), self.peek_u8(addr.wrapping_add(1))])
    }

    fn peek_u16_zero_page(&self, addr: u8) -> u16 {
        u16::from_le_bytes([
            self.peek_u8(addr.into()),
            self.peek_u8(addr.wrapping_add(1).into()),
        ])
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

    fn operand_address(&mut self, mode: &AddressingMode) -> Option<u16> {
        let addr = match mode {
            AddressingMode::None | AddressingMode::Relative | AddressingMode::Accumulator => {
                return None;
            }
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
                let lo = self.read_u8();
                let hi = self.read_u8();
                u16::from_le_bytes([
                    self.peek_u8(u16::from_le_bytes([lo, hi])),
                    self.peek_u8(u16::from_le_bytes([lo.wrapping_add(1), hi])),
                ])
            }
            AddressingMode::IndirectX => {
                let ptr = self.read_u8().wrapping_add(self.register_x);
                self.peek_u16_zero_page(ptr)
            }
            AddressingMode::IndirectY => {
                let base = self.read_u8();
                self.peek_u16_zero_page(base.into())
                    .wrapping_add(self.register_y.into())
            }
        };

        Some(addr)
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
                dbg!("break", self.program_counter);
                return;
            }

            callback(self);

            let opcode = self.read_u8();
            let opcode = match OP_CODES_MAP.get(&opcode) {
                Some(opcode) => opcode,
                None => todo!("{}", format!("opcode {:#02x?}", opcode)),
            };

            let addr = self.operand_address(&opcode.addressing_mode);
            (opcode.method)(self, addr);
        }
    }

    fn brk(&mut self, _addr: Option<u16>) {
        dbg!("brk", self.program_counter);
        self.status.insert(StatusFlags::BREAK_COMMAND);
    }

    fn rti(&mut self, _addr: Option<u16>) {
        let mut status = StatusFlags::from_bits_truncate(self.stack_pop_u8());
        // TODO: Actually implement this.
        let _ = self.stack_pop_u16();
        status -= StatusFlags::BREAK_COMMAND;
        status |= StatusFlags::UNUSED;
        self.status = status;
    }

    fn asl(&mut self, addr: Option<u16>) {
        let result = if let Some(addr) = addr {
            let value = self.peek_u8(addr);
            self.status
                .set(StatusFlags::CARRY, value & 0b1000_0000 != 0);
            let result = value << 1;
            self.write_u8(addr, result);
            result
        } else {
            self.status
                .set(StatusFlags::CARRY, self.register_a & 0b1000_0000 != 0);
            self.register_a <<= 1;
            self.register_a
        };

        self.update_zero_and_negative_flags(result);
    }

    fn rol(&mut self, addr: Option<u16>) {
        let result = if let Some(addr) = addr {
            let mut value = self.peek_u8(addr);

            let old_bit = value & 0b1000_0000;
            value <<= 1;
            value |= self.status.contains(StatusFlags::CARRY) as u8;
            self.status.set(StatusFlags::CARRY, old_bit != 0);

            self.write_u8(addr, value);
            value
        } else {
            let old_bit = self.register_a & 0b1000_0000;
            self.register_a <<= 1;
            self.register_a |= self.status.contains(StatusFlags::CARRY) as u8;
            self.status.set(StatusFlags::CARRY, old_bit != 0);
            self.register_a
        };

        self.update_zero_and_negative_flags(result);
    }

    fn ror(&mut self, addr: Option<u16>) {
        let result = if let Some(addr) = addr {
            let mut value = self.peek_u8(addr);

            let old_bit = value & 0b1;
            value >>= 1;
            value |= (self.status.contains(StatusFlags::CARRY) as u8).shl(7);
            self.status.set(StatusFlags::CARRY, old_bit != 0);

            self.write_u8(addr, value);
            value
        } else {
            let old_bit = self.register_a & 0b1;
            self.register_a >>= 1;
            self.register_a |= (self.status.contains(StatusFlags::CARRY) as u8).shl(7);
            self.status.set(StatusFlags::CARRY, old_bit != 0);
            self.register_a
        };

        self.update_zero_and_negative_flags(result);
    }

    fn nop(&mut self, _addr: Option<u16>) {}

    fn clv(&mut self, _addr: Option<u16>) {
        self.status.remove(StatusFlags::OVERFLOW);
    }

    fn cli(&mut self, _addr: Option<u16>) {
        self.status.remove(StatusFlags::INTERRUPT_DISABLE);
    }

    fn sei(&mut self, _addr: Option<u16>) {
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
    }

    fn cld(&mut self, _addr: Option<u16>) {
        self.status.remove(StatusFlags::DECIMAL_MODE);
    }

    fn sed(&mut self, _addr: Option<u16>) {
        self.status.insert(StatusFlags::DECIMAL_MODE);
    }

    fn clc(&mut self, _addr: Option<u16>) {
        self.status.remove(StatusFlags::CARRY);
    }

    fn sec(&mut self, _addr: Option<u16>) {
        self.status.insert(StatusFlags::CARRY);
    }

    fn iny(&mut self, _addr: Option<u16>) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn inx(&mut self, _addr: Option<u16>) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inc(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr).wrapping_add(1);
            self.write_u8(addr, value);
            self.update_zero_and_negative_flags(value);
        }
    }

    fn txa(&mut self, _addr: Option<u16>) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn tax(&mut self, _addr: Option<u16>) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self, _addr: Option<u16>) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn dex(&mut self, _addr: Option<u16>) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dec(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr).wrapping_sub(1);
            self.write_u8(addr, value);
            self.update_zero_and_negative_flags(value);
        }
    }

    fn branch(&mut self, condition: bool) {
        let value = self.read_i8();
        if condition {
            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
        }
    }

    fn bvs(&mut self, _addr: Option<u16>) {
        self.branch(self.status.contains(StatusFlags::OVERFLOW));
    }

    fn bvc(&mut self, _addr: Option<u16>) {
        self.branch(!self.status.contains(StatusFlags::OVERFLOW));
    }

    fn bcc(&mut self, _addr: Option<u16>) {
        self.branch(!self.status.contains(StatusFlags::CARRY));
    }

    fn bcs(&mut self, _addr: Option<u16>) {
        self.branch(self.status.contains(StatusFlags::CARRY));
    }

    fn bmi(&mut self, _addr: Option<u16>) {
        self.branch(self.status.contains(StatusFlags::NEGATIVE));
    }

    fn bpl(&mut self, _addr: Option<u16>) {
        self.branch(!self.status.contains(StatusFlags::NEGATIVE));
    }

    fn bne(&mut self, _addr: Option<u16>) {
        self.branch(!self.status.contains(StatusFlags::ZERO));
    }

    fn beq(&mut self, _addr: Option<u16>) {
        self.branch(self.status.contains(StatusFlags::ZERO));
    }

    fn jmp(&mut self, addr: Option<u16>) {
        self.program_counter = addr.unwrap();
    }

    /// test BITs
    fn bit(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);
            self.status
                .set(StatusFlags::ZERO, value & self.register_a == 0);
            self.status
                .set(StatusFlags::OVERFLOW, value & 0b0100_0000 != 0);
            self.status
                .set(StatusFlags::NEGATIVE, value & 0b1000_0000 != 0);
        }
    }

    fn cpy(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);
            let result = self.register_y.wrapping_sub(value);

            self.status
                .set(StatusFlags::CARRY, self.register_y >= value);
            self.update_zero_and_negative_flags(result);
        }
    }

    fn cpx(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);
            let result = self.register_x.wrapping_sub(value);

            self.status
                .set(StatusFlags::CARRY, self.register_x >= value);
            self.update_zero_and_negative_flags(result);
        }
    }

    fn cmp(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);
            let result = self.register_a.wrapping_sub(value);

            self.status
                .set(StatusFlags::CARRY, self.register_a >= value);
            self.update_zero_and_negative_flags(result);
        }
    }

    fn lsr(&mut self, addr: Option<u16>) {
        let result = if let Some(addr) = addr {
            let value = self.bus.mem_read(addr);
            self.status
                .set(StatusFlags::CARRY, value & 0b0000_0001 != 0);
            let result = value.wrapping_shr(1);
            self.write_u8(addr, result);
            result
        } else {
            self.status
                .set(StatusFlags::CARRY, self.register_a & 0b0000_0001 != 0);
            self.register_a = self.register_a.wrapping_shr(1);
            self.register_a
        };

        self.update_zero_and_negative_flags(result);
    }

    fn sbc(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);

            let borrow = !self.status.contains(StatusFlags::CARRY);
            // TODO: Use `borrowing_sub` when stable.
            let (_, overflow) = (self.register_a as i8).sub_borrowing(value as i8, borrow);
            let (result, borrow) = self.register_a.sub_borrowing(value, borrow);

            self.register_a = result;
            self.status.set(StatusFlags::CARRY, !borrow);
            self.status.set(StatusFlags::OVERFLOW, overflow);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    fn adc(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            let value = self.peek_u8(addr);

            let carry = self.status.contains(StatusFlags::CARRY);
            // TODO: Use `carrying_add` when stable.
            let (_, overflow) = (self.register_a as i8).add_carrying(value as i8, carry);
            let (result, carry) = self.register_a.add_carrying(value, carry);

            self.register_a = result;
            self.status.set(StatusFlags::CARRY, carry);
            self.status.set(StatusFlags::OVERFLOW, overflow);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    fn eor(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_a = self.register_a ^ self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    fn ora(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_a = self.register_a | self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    fn and(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_a = self.register_a & self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    fn rts(&mut self, _addr: Option<u16>) {
        self.program_counter = self.stack_pop_u16() + 1;
    }

    fn jsr(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.stack_push_u16(self.program_counter - 1);
            self.program_counter = addr;
        }
    }

    fn sta(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.write_u8(addr, self.register_a);
        }
    }

    fn stx(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.write_u8(addr, self.register_x);
        }
    }

    fn sty(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.write_u8(addr, self.register_y);
        }
    }

    fn ldy(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_y = self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_y);
        }
    }

    fn ldx(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_x = self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_x);
        }
    }

    fn lda(&mut self, addr: Option<u16>) {
        if let Some(addr) = addr {
            self.register_a = self.peek_u8(addr);
            self.update_zero_and_negative_flags(self.register_a);
        }
    }

    /// PuLl Accumulator
    fn pla(&mut self, _addr: Option<u16>) {
        self.register_a = self.stack_pop_u8();
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// PusH Accumulator
    fn pha(&mut self, _addr: Option<u16>) {
        self.stack_push_u8(self.register_a);
    }

    /// PuLl Processor status
    fn plp(&mut self, _addr: Option<u16>) {
        let mut status = StatusFlags::from_bits_truncate(self.stack_pop_u8());
        status -= StatusFlags::BREAK_COMMAND;
        status |= StatusFlags::UNUSED;
        self.status = status;
    }

    /// PusH Processor status
    fn php(&mut self, _addr: Option<u16>) {
        let result = self.status.bits() | StatusFlags::BREAK_COMMAND.bits();
        self.stack_push_u8(result);
    }

    /// Transfer X to Stack ptr
    fn txs(&mut self, _addr: Option<u16>) {
        self.stack_pointer = self.register_x;
    }

    /// Transfer Stack ptr to X
    fn tsx(&mut self, _addr: Option<u16>) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self, _addr: Option<u16>) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tya(&mut self, _addr: Option<u16>) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }
}

#[cfg(test)]
mod test {
    use crate::bus::Bus;
    use crate::cpu::{StatusFlags, CPU};
    use crate::rom::tests::test_rom;
    use crate::rom::Rom;

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

    #[test]
    fn nestest() {
        let nestest = include_bytes!("nestest.nes");
        let rom = Rom::new(&nestest.to_vec()).unwrap();
        let bus = Bus::new(rom);
        let mut cpu = CPU::new(bus);
        cpu.reset();
        cpu.program_counter = 0xc000;
        let expected: Vec<_> = include_str!("nestest_no_cycle.log").lines().collect();
        let mut i = 0;
        cpu.run_with_callback(|cpu| {
            dbg!(i);
            pretty_assertions::assert_eq!(format!("{cpu:?}"), expected[i]);
            i += 1;
        });
    }
}
