use crate::bus::Bus;
use crate::carrying::CarryingExt;
use bitflags::bitflags;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::ops::Shl;
use std::sync::LazyLock;

const STACK_BASE: u16 = 0x100;

const NMI_INTERRUPT: (u16, u16) = (0xfffa, 0xfffb);
const RESET_INTERRUPT: (u16, u16) = (0xfffc, 0xfffd);
const BRK_INTERRUPT: (u16, u16) = (0xfffe, 0xffff);

type OpcodeMethod = fn(&mut CPU, &Operand);

// References:
// * https://www.nesdev.org/obelisk-6502-guide/reference.html
// * http://www.textfiles.com/programming/opcod6502.txt
// * https://web.archive.org/web/20220831224234if_/https://users.telenet.be/kim1-6502/6502/proman.html
// * https://web.archive.org/web/20220831224314if_/http://users.telenet.be/kim1-6502/6502/pm-apndx.html
const INSTRUCTIONS: [Opcode; 231] = [
    Opcode::new(0x00, "BRK", AddressingMode::None, 7, false, CPU::brk),
    Opcode::new(0xea, "NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0x18, "CLC", AddressingMode::None, 2, false, CPU::clc),
    Opcode::new(0x38, "SEC", AddressingMode::None, 2, false, CPU::sec),
    Opcode::new(0xd8, "CLD", AddressingMode::None, 2, false, CPU::cld),
    Opcode::new(0xf8, "SED", AddressingMode::None, 2, false, CPU::sed),
    Opcode::new(0xb8, "CLV", AddressingMode::None, 2, false, CPU::clv),
    Opcode::new(0x58, "CLI", AddressingMode::None, 2, false, CPU::cli),
    Opcode::new(0x78, "SEI", AddressingMode::None, 2, false, CPU::sei),
    Opcode::new(0xaa, "TAX", AddressingMode::None, 2, false, CPU::tax),
    Opcode::new(0x8a, "TXA", AddressingMode::None, 2, false, CPU::txa),
    Opcode::new(0x9a, "TXS", AddressingMode::None, 2, false, CPU::txs),
    Opcode::new(0xba, "TSX", AddressingMode::None, 2, false, CPU::tsx),
    Opcode::new(0x20, "JSR", AddressingMode::Absolute, 6, false, CPU::jsr),
    Opcode::new(0xe8, "INX", AddressingMode::None, 2, false, CPU::inx),
    Opcode::new(0xc8, "INY", AddressingMode::None, 2, false, CPU::iny),
    Opcode::new(0x60, "RTS", AddressingMode::None, 6, false, CPU::rts),
    Opcode::new(0xe6, "INC", AddressingMode::ZeroPage, 5, false, CPU::inc),
    Opcode::new(0xf6, "INC", AddressingMode::ZeroPageX, 6, false, CPU::inc),
    Opcode::new(0xee, "INC", AddressingMode::Absolute, 6, false, CPU::inc),
    Opcode::new(0xfe, "INC", AddressingMode::AbsoluteX, 7, false, CPU::inc),
    Opcode::new(0xa9, "LDA", AddressingMode::Immediate, 2, false, CPU::lda),
    Opcode::new(0xa5, "LDA", AddressingMode::ZeroPage, 3, false, CPU::lda),
    Opcode::new(0xb5, "LDA", AddressingMode::ZeroPageX, 4, false, CPU::lda),
    Opcode::new(0xad, "LDA", AddressingMode::Absolute, 4, false, CPU::lda),
    Opcode::new(0xbd, "LDA", AddressingMode::AbsoluteX, 4, true, CPU::lda),
    Opcode::new(0xb9, "LDA", AddressingMode::AbsoluteY, 4, true, CPU::lda),
    Opcode::new(0xa1, "LDA", AddressingMode::IndirectX, 6, false, CPU::lda),
    Opcode::new(0xb1, "LDA", AddressingMode::IndirectY, 5, true, CPU::lda),
    Opcode::new(0xa2, "LDX", AddressingMode::Immediate, 2, false, CPU::ldx),
    Opcode::new(0xa6, "LDX", AddressingMode::ZeroPage, 3, false, CPU::ldx),
    Opcode::new(0xb6, "LDX", AddressingMode::ZeroPageY, 4, false, CPU::ldx),
    Opcode::new(0xae, "LDX", AddressingMode::Absolute, 4, false, CPU::ldx),
    Opcode::new(0xbe, "LDX", AddressingMode::AbsoluteY, 4, true, CPU::ldx),
    Opcode::new(0xa0, "LDY", AddressingMode::Immediate, 2, false, CPU::ldy),
    Opcode::new(0xa4, "LDY", AddressingMode::ZeroPage, 3, false, CPU::ldy),
    Opcode::new(0xb4, "LDY", AddressingMode::ZeroPageX, 4, false, CPU::ldy),
    Opcode::new(0xac, "LDY", AddressingMode::Absolute, 4, false, CPU::ldy),
    Opcode::new(0xbc, "LDY", AddressingMode::AbsoluteX, 4, true, CPU::ldy),
    Opcode::new(0x85, "STA", AddressingMode::ZeroPage, 3, false, CPU::sta),
    Opcode::new(0x95, "STA", AddressingMode::ZeroPageX, 4, false, CPU::sta),
    Opcode::new(0x8d, "STA", AddressingMode::Absolute, 4, false, CPU::sta),
    Opcode::new(0x9d, "STA", AddressingMode::AbsoluteX, 5, false, CPU::sta),
    Opcode::new(0x99, "STA", AddressingMode::AbsoluteY, 5, false, CPU::sta),
    Opcode::new(0x81, "STA", AddressingMode::IndirectX, 6, false, CPU::sta),
    Opcode::new(0x91, "STA", AddressingMode::IndirectY, 6, false, CPU::sta),
    Opcode::new(0x86, "STX", AddressingMode::ZeroPage, 3, false, CPU::stx),
    Opcode::new(0x96, "STX", AddressingMode::ZeroPageY, 4, false, CPU::stx),
    Opcode::new(0x8e, "STX", AddressingMode::Absolute, 4, false, CPU::stx),
    Opcode::new(0x84, "STY", AddressingMode::ZeroPage, 3, false, CPU::sty),
    Opcode::new(0x94, "STY", AddressingMode::ZeroPageX, 4, false, CPU::sty),
    Opcode::new(0x8c, "STY", AddressingMode::Absolute, 4, false, CPU::sty),
    Opcode::new(0x29, "AND", AddressingMode::Immediate, 2, false, CPU::and),
    Opcode::new(0x25, "AND", AddressingMode::ZeroPage, 3, false, CPU::and),
    Opcode::new(0x35, "AND", AddressingMode::ZeroPageX, 4, false, CPU::and),
    Opcode::new(0x2D, "AND", AddressingMode::Absolute, 4, false, CPU::and),
    Opcode::new(0x3D, "AND", AddressingMode::AbsoluteX, 4, true, CPU::and),
    Opcode::new(0x39, "AND", AddressingMode::AbsoluteY, 4, true, CPU::and),
    Opcode::new(0x21, "AND", AddressingMode::IndirectX, 6, false, CPU::and),
    Opcode::new(0x31, "AND", AddressingMode::IndirectY, 5, true, CPU::and),
    Opcode::new(0x49, "EOR", AddressingMode::Immediate, 2, false, CPU::eor),
    Opcode::new(0x45, "EOR", AddressingMode::ZeroPage, 3, false, CPU::eor),
    Opcode::new(0x55, "EOR", AddressingMode::ZeroPageX, 4, false, CPU::eor),
    Opcode::new(0x4D, "EOR", AddressingMode::Absolute, 4, false, CPU::eor),
    Opcode::new(0x5D, "EOR", AddressingMode::AbsoluteX, 4, true, CPU::eor),
    Opcode::new(0x59, "EOR", AddressingMode::AbsoluteY, 4, true, CPU::eor),
    Opcode::new(0x41, "EOR", AddressingMode::IndirectX, 6, false, CPU::eor),
    Opcode::new(0x51, "EOR", AddressingMode::IndirectY, 5, true, CPU::eor),
    Opcode::new(0x09, "ORA", AddressingMode::Immediate, 2, false, CPU::ora),
    Opcode::new(0x05, "ORA", AddressingMode::ZeroPage, 3, false, CPU::ora),
    Opcode::new(0x15, "ORA", AddressingMode::ZeroPageX, 4, false, CPU::ora),
    Opcode::new(0x0d, "ORA", AddressingMode::Absolute, 4, false, CPU::ora),
    Opcode::new(0x1d, "ORA", AddressingMode::AbsoluteX, 4, true, CPU::ora),
    Opcode::new(0x19, "ORA", AddressingMode::AbsoluteY, 4, true, CPU::ora),
    Opcode::new(0x01, "ORA", AddressingMode::IndirectX, 6, false, CPU::ora),
    Opcode::new(0x11, "ORA", AddressingMode::IndirectY, 5, true, CPU::ora),
    Opcode::new(0x69, "ADC", AddressingMode::Immediate, 2, false, CPU::adc),
    Opcode::new(0x65, "ADC", AddressingMode::ZeroPage, 3, false, CPU::adc),
    Opcode::new(0x75, "ADC", AddressingMode::ZeroPageX, 4, false, CPU::adc),
    Opcode::new(0x6d, "ADC", AddressingMode::Absolute, 4, false, CPU::adc),
    Opcode::new(0x7d, "ADC", AddressingMode::AbsoluteX, 4, true, CPU::adc),
    Opcode::new(0x79, "ADC", AddressingMode::AbsoluteY, 4, true, CPU::adc),
    Opcode::new(0x61, "ADC", AddressingMode::IndirectX, 6, false, CPU::adc),
    Opcode::new(0x71, "ADC", AddressingMode::IndirectY, 5, true, CPU::adc),
    Opcode::new(0xe9, "SBC", AddressingMode::Immediate, 2, false, CPU::sbc),
    Opcode::new(0xe5, "SBC", AddressingMode::ZeroPage, 3, false, CPU::sbc),
    Opcode::new(0xf5, "SBC", AddressingMode::ZeroPageX, 4, false, CPU::sbc),
    Opcode::new(0xed, "SBC", AddressingMode::Absolute, 4, false, CPU::sbc),
    Opcode::new(0xfd, "SBC", AddressingMode::AbsoluteX, 4, true, CPU::sbc),
    Opcode::new(0xf9, "SBC", AddressingMode::AbsoluteY, 4, true, CPU::sbc),
    Opcode::new(0xe1, "SBC", AddressingMode::IndirectX, 6, false, CPU::sbc),
    Opcode::new(0xf1, "SBC", AddressingMode::IndirectY, 5, true, CPU::sbc),
    Opcode::new(0xc9, "CMP", AddressingMode::Immediate, 2, false, CPU::cmp),
    Opcode::new(0xc5, "CMP", AddressingMode::ZeroPage, 3, false, CPU::cmp),
    Opcode::new(0xd5, "CMP", AddressingMode::ZeroPageX, 4, false, CPU::cmp),
    Opcode::new(0xcd, "CMP", AddressingMode::Absolute, 4, false, CPU::cmp),
    Opcode::new(0xdd, "CMP", AddressingMode::AbsoluteX, 4, true, CPU::cmp),
    Opcode::new(0xd9, "CMP", AddressingMode::AbsoluteY, 4, true, CPU::cmp),
    Opcode::new(0xc1, "CMP", AddressingMode::IndirectX, 6, false, CPU::cmp),
    Opcode::new(0xd1, "CMP", AddressingMode::IndirectY, 5, true, CPU::cmp),
    Opcode::new(0xe0, "CPX", AddressingMode::Immediate, 2, false, CPU::cpx),
    Opcode::new(0xe4, "CPX", AddressingMode::ZeroPage, 3, false, CPU::cpx),
    Opcode::new(0xec, "CPX", AddressingMode::Absolute, 4, false, CPU::cpx),
    Opcode::new(0xc0, "CPY", AddressingMode::Immediate, 2, false, CPU::cpy),
    Opcode::new(0xc4, "CPY", AddressingMode::ZeroPage, 3, false, CPU::cpy),
    Opcode::new(0xcc, "CPY", AddressingMode::Absolute, 4, false, CPU::cpy),
    Opcode::new(0xf0, "BEQ", AddressingMode::Relative, 2, true, CPU::beq),
    Opcode::new(0xd0, "BNE", AddressingMode::Relative, 2, false, CPU::bne),
    Opcode::new(0xb0, "BCS", AddressingMode::Relative, 2, false, CPU::bcs),
    Opcode::new(0x90, "BCC", AddressingMode::Relative, 2, false, CPU::bcc),
    Opcode::new(0x10, "BPL", AddressingMode::Relative, 2, false, CPU::bpl),
    Opcode::new(0x30, "BMI", AddressingMode::Relative, 2, false, CPU::bmi),
    Opcode::new(0x50, "BVC", AddressingMode::Relative, 2, false, CPU::bvc),
    Opcode::new(0x70, "BVS", AddressingMode::Relative, 2, false, CPU::bvs),
    Opcode::new(0xc6, "DEC", AddressingMode::ZeroPage, 5, false, CPU::dec),
    Opcode::new(0xd6, "DEC", AddressingMode::ZeroPageX, 6, false, CPU::dec),
    Opcode::new(0xce, "DEC", AddressingMode::Absolute, 6, false, CPU::dec),
    Opcode::new(0xde, "DEC", AddressingMode::AbsoluteX, 7, false, CPU::dec),
    Opcode::new(0xca, "DEX", AddressingMode::None, 2, false, CPU::dex),
    Opcode::new(0x88, "DEY", AddressingMode::None, 2, false, CPU::dey),
    Opcode::new(0x4a, "LSR", AddressingMode::Accumulator, 2, false, CPU::lsr),
    Opcode::new(0x46, "LSR", AddressingMode::ZeroPage, 5, false, CPU::lsr),
    Opcode::new(0x56, "LSR", AddressingMode::ZeroPageX, 6, false, CPU::lsr),
    Opcode::new(0x4e, "LSR", AddressingMode::Absolute, 6, false, CPU::lsr),
    Opcode::new(0x5e, "LSR", AddressingMode::AbsoluteX, 7, false, CPU::lsr),
    Opcode::new(0x24, "BIT", AddressingMode::ZeroPage, 3, false, CPU::bit),
    Opcode::new(0x2c, "BIT", AddressingMode::Absolute, 4, false, CPU::bit),
    Opcode::new(0x4c, "JMP", AddressingMode::Absolute, 3, false, CPU::jmp),
    Opcode::new(0x6c, "JMP", AddressingMode::Indirect, 5, false, CPU::jmp),
    Opcode::new(0x48, "PHA", AddressingMode::None, 3, false, CPU::pha),
    Opcode::new(0x68, "PLA", AddressingMode::None, 4, false, CPU::pla),
    Opcode::new(0x08, "PHP", AddressingMode::None, 3, false, CPU::php),
    Opcode::new(0x28, "PLP", AddressingMode::None, 4, false, CPU::plp),
    Opcode::new(0xa8, "TAY", AddressingMode::None, 2, false, CPU::tay),
    Opcode::new(0x98, "TYA", AddressingMode::None, 2, false, CPU::tya),
    Opcode::new(0x40, "RTI", AddressingMode::None, 6, false, CPU::rti),
    Opcode::new(0x0a, "ASL", AddressingMode::Accumulator, 2, false, CPU::asl),
    Opcode::new(0x06, "ASL", AddressingMode::ZeroPage, 5, false, CPU::asl),
    Opcode::new(0x16, "ASL", AddressingMode::ZeroPageX, 6, false, CPU::asl),
    Opcode::new(0x0e, "ASL", AddressingMode::Absolute, 6, false, CPU::asl),
    Opcode::new(0x1e, "ASL", AddressingMode::AbsoluteX, 7, false, CPU::asl),
    Opcode::new(0x2a, "ROL", AddressingMode::Accumulator, 2, false, CPU::rol),
    Opcode::new(0x26, "ROL", AddressingMode::ZeroPage, 5, false, CPU::rol),
    Opcode::new(0x36, "ROL", AddressingMode::ZeroPageX, 6, false, CPU::rol),
    Opcode::new(0x2e, "ROL", AddressingMode::Absolute, 6, false, CPU::rol),
    Opcode::new(0x3e, "ROL", AddressingMode::AbsoluteX, 7, false, CPU::rol),
    Opcode::new(0x6a, "ROR", AddressingMode::Accumulator, 2, false, CPU::ror),
    Opcode::new(0x66, "ROR", AddressingMode::ZeroPage, 5, false, CPU::ror),
    Opcode::new(0x76, "ROR", AddressingMode::ZeroPageX, 6, false, CPU::ror),
    Opcode::new(0x6e, "ROR", AddressingMode::Absolute, 6, false, CPU::ror),
    Opcode::new(0x7e, "ROR", AddressingMode::AbsoluteX, 7, false, CPU::ror),
    Opcode::new(0x1a, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0x3a, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0x5a, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0x7a, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0xda, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0xfa, "*NOP", AddressingMode::None, 2, false, CPU::nop),
    Opcode::new(0x04, "*NOP", AddressingMode::ZeroPage, 3, false, CPU::nop),
    Opcode::new(0x14, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0x34, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0x44, "*NOP", AddressingMode::ZeroPage, 3, false, CPU::nop),
    Opcode::new(0x54, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0x64, "*NOP", AddressingMode::ZeroPage, 3, false, CPU::nop),
    Opcode::new(0x74, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0x80, "*NOP", AddressingMode::Immediate, 2, false, CPU::nop),
    Opcode::new(0x82, "*NOP", AddressingMode::Immediate, 2, false, CPU::nop),
    Opcode::new(0x89, "*NOP", AddressingMode::Immediate, 2, false, CPU::nop),
    Opcode::new(0xc2, "*NOP", AddressingMode::Immediate, 2, false, CPU::nop),
    Opcode::new(0xd4, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0xe2, "*NOP", AddressingMode::Immediate, 2, false, CPU::nop),
    Opcode::new(0xf4, "*NOP", AddressingMode::ZeroPageX, 4, false, CPU::nop),
    Opcode::new(0x0c, "*NOP", AddressingMode::Absolute, 4, false, CPU::nop),
    Opcode::new(0x1c, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0x3c, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0x5c, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0x7c, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0xdc, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0xfc, "*NOP", AddressingMode::AbsoluteX, 4, true, CPU::nop),
    Opcode::new(0xa7, "*LAX", AddressingMode::ZeroPage, 3, false, CPU::lax),
    Opcode::new(0xb7, "*LAX", AddressingMode::ZeroPageY, 4, false, CPU::lax),
    Opcode::new(0xaf, "*LAX", AddressingMode::Absolute, 4, false, CPU::lax),
    Opcode::new(0xbf, "*LAX", AddressingMode::AbsoluteY, 4, true, CPU::lax),
    Opcode::new(0xa3, "*LAX", AddressingMode::IndirectX, 6, false, CPU::lax),
    Opcode::new(0xb3, "*LAX", AddressingMode::IndirectY, 5, true, CPU::lax),
    Opcode::new(0x87, "*SAX", AddressingMode::ZeroPage, 3, false, CPU::sax),
    Opcode::new(0x97, "*SAX", AddressingMode::ZeroPageY, 4, false, CPU::sax),
    Opcode::new(0x83, "*SAX", AddressingMode::IndirectX, 6, false, CPU::sax),
    Opcode::new(0x8f, "*SAX", AddressingMode::Absolute, 4, false, CPU::sax),
    Opcode::new(0xeb, "*SBC", AddressingMode::Immediate, 2, false, CPU::sbc),
    Opcode::new(0xc7, "*DCP", AddressingMode::ZeroPage, 5, false, CPU::dcp),
    Opcode::new(0xd7, "*DCP", AddressingMode::ZeroPageX, 6, false, CPU::dcp),
    Opcode::new(0xcf, "*DCP", AddressingMode::Absolute, 6, false, CPU::dcp),
    Opcode::new(0xdf, "*DCP", AddressingMode::AbsoluteX, 7, false, CPU::dcp),
    Opcode::new(0xdb, "*DCP", AddressingMode::AbsoluteY, 7, false, CPU::dcp),
    Opcode::new(0xc3, "*DCP", AddressingMode::IndirectX, 8, false, CPU::dcp),
    Opcode::new(0xd3, "*DCP", AddressingMode::IndirectY, 8, false, CPU::dcp),
    Opcode::new(0xe7, "*ISB", AddressingMode::ZeroPage, 5, false, CPU::isb),
    Opcode::new(0xf7, "*ISB", AddressingMode::ZeroPageX, 6, false, CPU::isb),
    Opcode::new(0xef, "*ISB", AddressingMode::Absolute, 6, false, CPU::isb),
    Opcode::new(0xff, "*ISB", AddressingMode::AbsoluteX, 7, false, CPU::isb),
    Opcode::new(0xfb, "*ISB", AddressingMode::AbsoluteY, 7, false, CPU::isb),
    Opcode::new(0xe3, "*ISB", AddressingMode::IndirectX, 8, false, CPU::isb),
    Opcode::new(0xf3, "*ISB", AddressingMode::IndirectY, 8, false, CPU::isb),
    Opcode::new(0x07, "*SLO", AddressingMode::ZeroPage, 5, false, CPU::slo),
    Opcode::new(0x17, "*SLO", AddressingMode::ZeroPageX, 6, false, CPU::slo),
    Opcode::new(0x0f, "*SLO", AddressingMode::Absolute, 6, false, CPU::slo),
    Opcode::new(0x1f, "*SLO", AddressingMode::AbsoluteX, 7, false, CPU::slo),
    Opcode::new(0x1b, "*SLO", AddressingMode::AbsoluteY, 7, false, CPU::slo),
    Opcode::new(0x03, "*SLO", AddressingMode::IndirectX, 8, false, CPU::slo),
    Opcode::new(0x13, "*SLO", AddressingMode::IndirectY, 8, false, CPU::slo),
    Opcode::new(0x27, "*RLA", AddressingMode::ZeroPage, 5, false, CPU::rla),
    Opcode::new(0x37, "*RLA", AddressingMode::ZeroPageX, 6, false, CPU::rla),
    Opcode::new(0x2f, "*RLA", AddressingMode::Absolute, 6, false, CPU::rla),
    Opcode::new(0x3f, "*RLA", AddressingMode::AbsoluteX, 7, false, CPU::rla),
    Opcode::new(0x3b, "*RLA", AddressingMode::AbsoluteY, 7, false, CPU::rla),
    Opcode::new(0x23, "*RLA", AddressingMode::IndirectX, 8, false, CPU::rla),
    Opcode::new(0x33, "*RLA", AddressingMode::IndirectY, 8, false, CPU::rla),
    Opcode::new(0x47, "*SRE", AddressingMode::ZeroPage, 5, false, CPU::sre),
    Opcode::new(0x57, "*SRE", AddressingMode::ZeroPageX, 6, false, CPU::sre),
    Opcode::new(0x4f, "*SRE", AddressingMode::Absolute, 6, false, CPU::sre),
    Opcode::new(0x5f, "*SRE", AddressingMode::AbsoluteX, 7, false, CPU::sre),
    Opcode::new(0x5b, "*SRE", AddressingMode::AbsoluteY, 7, false, CPU::sre),
    Opcode::new(0x43, "*SRE", AddressingMode::IndirectX, 8, false, CPU::sre),
    Opcode::new(0x53, "*SRE", AddressingMode::IndirectY, 8, false, CPU::sre),
    Opcode::new(0x67, "*RRA", AddressingMode::ZeroPage, 5, false, CPU::rra),
    Opcode::new(0x77, "*RRA", AddressingMode::ZeroPageX, 6, false, CPU::rra),
    Opcode::new(0x6f, "*RRA", AddressingMode::Absolute, 6, false, CPU::rra),
    Opcode::new(0x7f, "*RRA", AddressingMode::AbsoluteX, 7, false, CPU::rra),
    Opcode::new(0x7b, "*RRA", AddressingMode::AbsoluteY, 7, false, CPU::rra),
    Opcode::new(0x63, "*RRA", AddressingMode::IndirectX, 8, false, CPU::rra),
    Opcode::new(0x73, "*RRA", AddressingMode::IndirectY, 8, false, CPU::rra),
];

static OP_CODES_MAP: LazyLock<HashMap<u8, Opcode>> =
    LazyLock::new(|| HashMap::from_iter(INSTRUCTIONS.into_iter().map(|e| (e.opcode, e))));

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

#[derive(Clone)]
pub struct Opcode {
    opcode: u8,
    name: &'static str,
    mode: AddressingMode,
    cycles: u8,

    /// Whether this opcode should take one more cycle to execute if page boundary is crossed.
    page_cycles: bool,

    method: OpcodeMethod,
}

impl Opcode {
    const fn new(
        opcode: u8,
        name: &'static str,
        mode: AddressingMode,
        cycles: u8,
        page_cycles: bool,
        method: OpcodeMethod,
    ) -> Self {
        Self {
            opcode,
            name,
            mode,
            cycles,
            page_cycles,
            method,
        }
    }

    const fn size(&self) -> usize {
        self.mode.size()
    }
}

struct Operand {
    data: u16,
    base: Option<u16>,
    mode: AddressingMode,
    crossed_page: bool,
}

impl Operand {
    const fn new(data: u16, base: Option<u16>, mode: AddressingMode, crossed_page: bool) -> Self {
        Self {
            data,
            base,
            mode,
            crossed_page,
        }
    }

    fn get_data(&self, cpu: &mut CPU) -> u8 {
        if self.crossed_page {
            cpu.bus.tick(1);
        }

        if self.mode == AddressingMode::Immediate || self.mode == AddressingMode::Accumulator {
            self.data as u8
        } else {
            cpu.bus.read(self.data)
        }
    }
}

struct Instruction {
    opcode: Opcode,
    operand: Operand,
}

impl Instruction {
    fn new(opcode: Opcode, operand: Operand) -> Self {
        Self { opcode, operand }
    }

    fn execute(&self, cpu: &mut CPU) {
        (self.opcode.method)(cpu, &self.operand);
        cpu.bus.tick(self.opcode.cycles);
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl AddressingMode {
    const fn size(&self) -> usize {
        match self {
            Self::Indirect | Self::Absolute | Self::AbsoluteX | Self::AbsoluteY => 3,
            Self::IndirectY
            | Self::IndirectX
            | Self::ZeroPageY
            | Self::ZeroPageX
            | Self::ZeroPage
            | Self::Immediate
            | Self::Relative => 2,
            _ => 1,
        }
    }
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
            stack_pointer: 0,
            program_counter: 0xff,
            bus,
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;

        let _ = self.bus.read(STACK_BASE + self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);

        let _ = self.bus.read(STACK_BASE + self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);

        let _ = self.bus.read(STACK_BASE + self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);

        self.bus.tick(7);

        self.status.insert(StatusFlags::INTERRUPT_DISABLE);

        // Set `program_counter` to the reset vector at $fffc–$fffd.
        self.program_counter = u16::from_le_bytes([
            self.bus.read(RESET_INTERRUPT.0),
            self.bus.read(RESET_INTERRUPT.1),
        ]);
    }

    fn stack_push_u8(&mut self, data: u8) {
        self.bus.write(STACK_BASE + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u8(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.bus.read(STACK_BASE + self.stack_pointer as u16)
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

    fn interrupt_nmi(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.stack_push_u8(self.status.bits());
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
        self.bus.tick(2);

        // Set `program_counter` to the nmi vector at $fffa–$fffb.
        self.program_counter = u16::from_le_bytes([
            self.bus.read(NMI_INTERRUPT.0),
            self.bus.read(NMI_INTERRUPT.1),
        ]);
    }

    #[cfg(test)]
    fn load(&mut self, data: Vec<u8>) {
        for i in 0..data.len() as u16 {
            self.bus.write(0x600 + i, data[i as usize]);
        }
    }

    #[cfg(test)]
    fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&str),
    {
        loop {
            if self.status.contains(StatusFlags::BREAK_COMMAND) {
                return;
            }

            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt_nmi();
            }

            let instrcution = self.fetch_instruction();
            let debug = self.debug(&instrcution);

            self.program_counter = self
                .program_counter
                .wrapping_add(instrcution.opcode.size() as u16);

            instrcution.execute(self);

            callback(&debug);
        }
    }

    fn debug(&mut self, instruction: &Instruction) -> String {
        let mut bytes: Vec<u8> = Vec::with_capacity(instruction.opcode.size());
        for i in 0..instruction.opcode.size() {
            bytes.push(self.bus.read(self.program_counter + i as u16))
        }
        let (_, operand_bytes) = bytes.split_at(1);

        let operand = self.effective_address(&instruction.opcode, &operand_bytes);
        let addr = operand.data;
        let base = operand.base;

        let operand = match instruction.opcode.mode {
            AddressingMode::None => "".to_string(),
            AddressingMode::Accumulator => "A".to_string(),
            AddressingMode::Relative => format!("${addr:04X}"),
            AddressingMode::Immediate => format!("#${addr:02X}"),
            AddressingMode::ZeroPage => format!("${addr:02X} = {:02X}", self.bus.read(addr)),
            AddressingMode::ZeroPageX => {
                format!(
                    "${:02X},X @ {addr:02X} = {:02X}",
                    operand_bytes[0],
                    self.bus.read(addr)
                )
            }
            AddressingMode::ZeroPageY => {
                format!(
                    "${:02X},Y @ {addr:02X} = {:02X}",
                    operand_bytes[0],
                    self.bus.read(addr)
                )
            }
            AddressingMode::Absolute => {
                if instruction.opcode.name.starts_with("J") || (0x2000..0x3fff).contains(&addr) {
                    format!("${addr:04X}")
                } else {
                    format!("${addr:04X} = {:02X}", self.bus.read(addr))
                }
            }
            AddressingMode::AbsoluteX => {
                format!(
                    "${:04X},X @ {addr:04X} = {:02X}",
                    base.unwrap(),
                    self.bus.read(addr)
                )
            }
            AddressingMode::AbsoluteY => {
                format!(
                    "${:04X},Y @ {addr:04X} = {:02X}",
                    base.unwrap(),
                    self.bus.read(addr)
                )
            }
            AddressingMode::Indirect => {
                format!(
                    "(${:04X}) = {addr:04X}",
                    u16::from_le_bytes(operand_bytes.try_into().unwrap())
                )
            }
            AddressingMode::IndirectX => {
                format!(
                    "(${:02X},X) @ {:02X} = {addr:04X} = {:02X}",
                    operand_bytes[0],
                    base.unwrap(),
                    self.bus.read(addr)
                )
            }
            AddressingMode::IndirectY => {
                format!(
                    "(${:02X}),Y = {:04X} @ {addr:04X} = {:02X}",
                    operand_bytes[0],
                    base.unwrap(),
                    self.bus.read(addr)
                )
            }
        };

        let hex = bytes
            .iter()
            .map(|byte| format!("{byte:02X}",))
            .collect::<Vec<String>>()
            .join(" ");

        format!(
            "{:04X}  {hex:<8} {:>4} {operand:<27} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}",
            self.program_counter,
            instruction.opcode.name,
            self.register_a,
            self.register_x,
            self.register_y,
            self.status.bits(),
            self.stack_pointer,
            self.bus.ppu.scanline,
            self.bus.ppu.cycles,
            self.bus.cycles
        )
    }

    fn effective_address(&mut self, opcode: &Opcode, operand: &[u8]) -> Operand {
        let (data, base, carry) = match opcode.mode {
            AddressingMode::None => (0, None, false),
            AddressingMode::Accumulator => (self.register_a.into(), None, false),
            AddressingMode::Relative => {
                let [lo, hi] = u16::to_le_bytes(self.program_counter + opcode.size() as u16);
                let (addr_lo, carry) = lo.overflowing_add_signed(operand[0] as i8);
                let addr = u16::from_le_bytes([addr_lo, hi.wrapping_add(carry.into())]);

                (addr, None, carry)
            }
            AddressingMode::Immediate => {
                let addr = operand[0] as u16;

                (addr, None, false)
            }
            AddressingMode::ZeroPage => {
                let addr = operand[0] as u16;

                (addr, None, false)
            }
            AddressingMode::ZeroPageX => {
                let byte = operand[0];
                let addr = byte.wrapping_add(self.register_x) as u16;

                (addr, None, false)
            }
            AddressingMode::ZeroPageY => {
                let byte = operand[0];
                let addr = byte.wrapping_add(self.register_y) as u16;

                (addr, None, false)
            }
            AddressingMode::Absolute => {
                let addr = u16::from_le_bytes(operand.to_owned().try_into().unwrap());

                (addr, None, false)
            }
            AddressingMode::AbsoluteX => {
                let [lo, hi]: [u8; 2] = operand.to_owned().try_into().unwrap();
                let base = u16::from_le_bytes([lo, hi]);
                let (lo, carry) = lo.overflowing_add(self.register_x.into());
                let addr = u16::from_le_bytes([lo, hi.wrapping_add(carry.into())]);

                (addr, Some(base), carry)
            }
            AddressingMode::AbsoluteY => {
                let [lo, hi]: [u8; 2] = operand.to_owned().try_into().unwrap();
                let base = u16::from_le_bytes([lo, hi]);
                let (lo, carry) = lo.overflowing_add(self.register_y.into());
                let addr = u16::from_le_bytes([lo, hi.wrapping_add(carry.into())]);

                (addr, Some(base), carry)
            }
            AddressingMode::Indirect => {
                let [a, b]: [u8; 2] = operand.to_owned().try_into().unwrap();
                let addr = u16::from_le_bytes([
                    self.bus.read(u16::from_le_bytes([a, b])),
                    self.bus.read(u16::from_le_bytes([a.wrapping_add(1), b])),
                ]);

                (addr, None, false)
            }
            AddressingMode::IndirectX => {
                let ptr = operand[0].wrapping_add(self.register_x);
                let addr = u16::from_le_bytes([
                    self.bus.read(ptr.into()),
                    self.bus.read(ptr.wrapping_add(1).into()),
                ]);

                (addr, Some(ptr.into()), false)
            }
            AddressingMode::IndirectY => {
                let byte = operand[0];
                let lo = self.bus.read(byte.into());
                let hi = self.bus.read(byte.wrapping_add(1).into());
                let base = u16::from_le_bytes([lo, hi]);
                let (addr_lo, carry) = lo.overflowing_add(self.register_y);
                let addr = u16::from_le_bytes([addr_lo, hi.wrapping_add(carry.into())]);

                (addr, Some(base), carry)
            }
        };

        Operand::new(data, base, opcode.mode, carry && opcode.page_cycles)
    }

    fn fetch_instruction(&mut self) -> Instruction {
        let opcode = self.bus.read(self.program_counter);
        let opcode = match OP_CODES_MAP.get(&opcode) {
            Some(opcode) => opcode.to_owned(),
            None => todo!("opcode {opcode:#02x?}"),
        };

        let mut operand = Vec::with_capacity(opcode.size() - 1);
        for i in 0..(opcode.size() - 1) {
            operand.push(
                self.bus
                    .read(self.program_counter.wrapping_add(1 + i as u16)),
            );
        }

        let operand = self.effective_address(&opcode, &operand);

        Instruction::new(opcode, operand)
    }

    /// Force Break
    fn brk(&mut self, _operand: &Operand) {
        self.status.insert(StatusFlags::BREAK_COMMAND);
    }

    /// Return from interrupt
    fn rti(&mut self, _operand: &Operand) {
        let mut status = StatusFlags::from_bits_truncate(self.stack_pop_u8());
        // TODO: Actually implement this.
        let _ = self.stack_pop_u16();
        status -= StatusFlags::BREAK_COMMAND;
        status |= StatusFlags::UNUSED;
        self.status = status;
    }

    /// Shift Left One Bit (Memory or Accumulator)
    fn asl(&mut self, operand: &Operand) {
        let value = operand.get_data(self);
        self.status
            .set(StatusFlags::CARRY, value & 0b1000_0000 != 0);

        let result = value << 1;

        if operand.mode == AddressingMode::Accumulator {
            self.register_a = result;
        } else {
            self.bus.write(operand.data, result);
        }

        self.update_zero_and_negative_flags(result);
    }

    /// Rotate one bit left (memory or accumulator)
    fn rol(&mut self, operand: &Operand) {
        if operand.mode == AddressingMode::Accumulator {
            let old_bit = self.register_a & 0b1000_0000;
            self.register_a <<= 1;
            self.register_a |= self.status.contains(StatusFlags::CARRY) as u8;
            self.status.set(StatusFlags::CARRY, old_bit != 0);
            self.update_zero_and_negative_flags(self.register_a);
        } else {
            let mut value = operand.get_data(self);

            let old_bit = value & 0b1000_0000;
            value <<= 1;
            value |= self.status.contains(StatusFlags::CARRY) as u8;
            self.status.set(StatusFlags::CARRY, old_bit != 0);

            self.bus.write(operand.data, value);
            self.update_zero_and_negative_flags(value);
        }
    }

    /// Rotate one bit right (memory or accumulator)
    fn ror(&mut self, operand: &Operand) {
        if operand.mode == AddressingMode::Accumulator {
            let old_bit = self.register_a & 1;
            self.register_a >>= 1;
            self.register_a |= (self.status.contains(StatusFlags::CARRY) as u8).shl(7);
            self.status.set(StatusFlags::CARRY, old_bit != 0);
            self.update_zero_and_negative_flags(self.register_a);
        } else {
            let mut value = operand.get_data(self);

            let old_bit = value & 1;
            value >>= 1;
            value |= (self.status.contains(StatusFlags::CARRY) as u8).shl(7);
            self.status.set(StatusFlags::CARRY, old_bit != 0);

            self.bus.write(operand.data, value);
            self.update_zero_and_negative_flags(value);
        }
    }

    /// No operation
    fn nop(&mut self, operand: &Operand) {
        let _ = operand.get_data(self);
    }

    /// Clear overflow flag
    fn clv(&mut self, _operand: &Operand) {
        self.status.remove(StatusFlags::OVERFLOW);
    }

    /// Clear interrupt disable bit
    fn cli(&mut self, _operand: &Operand) {
        self.status.remove(StatusFlags::INTERRUPT_DISABLE);
    }

    /// Set interrupt disable status
    fn sei(&mut self, _operand: &Operand) {
        self.status.insert(StatusFlags::INTERRUPT_DISABLE);
    }

    /// Clear decimal mode
    fn cld(&mut self, _operand: &Operand) {
        self.status.remove(StatusFlags::DECIMAL_MODE);
    }

    /// Set decimal mode
    fn sed(&mut self, _operand: &Operand) {
        self.status.insert(StatusFlags::DECIMAL_MODE);
    }

    /// Clear carry flag
    fn clc(&mut self, _operand: &Operand) {
        self.status.remove(StatusFlags::CARRY);
    }

    /// Set carry flag
    fn sec(&mut self, _operand: &Operand) {
        self.status.insert(StatusFlags::CARRY);
    }

    /// Increment Index Y by one
    fn iny(&mut self, _operand: &Operand) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Increment Index X by one
    fn inx(&mut self, _operand: &Operand) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Increment memory by one
    fn inc(&mut self, operand: &Operand) {
        let value = operand.get_data(self).wrapping_add(1);
        self.bus.write(operand.data, value);
        self.update_zero_and_negative_flags(value);
    }

    /// Transfer index X to accumulator
    fn txa(&mut self, _operand: &Operand) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Transfer accumulator to index X
    fn tax(&mut self, _operand: &Operand) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Decrement index Y by one
    fn dey(&mut self, _operand: &Operand) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Decrement index X by one
    fn dex(&mut self, _operand: &Operand) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Decrement memory by one
    fn dec(&mut self, operand: &Operand) {
        let value = operand.get_data(self).wrapping_sub(1);
        self.bus.write(operand.data, value);
        self.update_zero_and_negative_flags(value);
    }

    fn branch(&mut self, operand: &Operand, condition: bool) {
        if condition {
            self.bus.tick(1);
            if operand.crossed_page {
                self.bus.tick(1);
            }
            self.program_counter = operand.data;
        }
    }

    /// Branch on overflow set
    fn bvs(&mut self, operand: &Operand) {
        self.branch(operand, self.status.contains(StatusFlags::OVERFLOW))
    }

    /// Branch on overflow clear
    fn bvc(&mut self, operand: &Operand) {
        self.branch(operand, !self.status.contains(StatusFlags::OVERFLOW))
    }

    /// Branch on Carry Clear
    fn bcc(&mut self, operand: &Operand) {
        self.branch(operand, !self.status.contains(StatusFlags::CARRY))
    }

    /// Branch on Carry Set
    fn bcs(&mut self, operand: &Operand) {
        self.branch(operand, self.status.contains(StatusFlags::CARRY))
    }

    /// Branch on result minus.
    fn bmi(&mut self, operand: &Operand) {
        self.branch(operand, self.status.contains(StatusFlags::NEGATIVE))
    }

    /// Branch on result plus.
    fn bpl(&mut self, operand: &Operand) {
        self.branch(operand, !self.status.contains(StatusFlags::NEGATIVE))
    }

    /// Branch on result not zero.
    fn bne(&mut self, operand: &Operand) {
        self.branch(operand, !self.status.contains(StatusFlags::ZERO))
    }

    /// Branch on result zero.
    fn beq(&mut self, operand: &Operand) {
        self.branch(operand, self.status.contains(StatusFlags::ZERO))
    }

    /// Jump to new location.
    fn jmp(&mut self, operand: &Operand) {
        self.program_counter = operand.data;
    }

    /// Test bits in memory with accumulator.
    fn bit(&mut self, operand: &Operand) {
        let value = operand.get_data(self);
        self.status
            .set(StatusFlags::ZERO, value & self.register_a == 0);
        self.status
            .set(StatusFlags::OVERFLOW, value & 0b0100_0000 != 0);
        self.status
            .set(StatusFlags::NEGATIVE, value & 0b1000_0000 != 0);
    }

    fn cpy(&mut self, operand: &Operand) {
        let value = operand.get_data(self);
        let result = self.register_y.wrapping_sub(value);

        self.status
            .set(StatusFlags::CARRY, self.register_y >= value);
        self.update_zero_and_negative_flags(result);
    }

    /// Compare Memory and Index X
    fn cpx(&mut self, operand: &Operand) {
        let value = operand.get_data(self);
        let result = self.register_x.wrapping_sub(value);

        self.status
            .set(StatusFlags::CARRY, self.register_x >= value);
        self.update_zero_and_negative_flags(result);
    }

    fn cmp(&mut self, operand: &Operand) {
        let value = operand.get_data(self);
        let result = self.register_a.wrapping_sub(value);

        self.status
            .set(StatusFlags::CARRY, self.register_a >= value);
        self.update_zero_and_negative_flags(result);
    }

    /// Shift right one bit (memory or accumulator)
    fn lsr(&mut self, operand: &Operand) {
        if operand.mode == AddressingMode::Accumulator {
            self.status
                .set(StatusFlags::CARRY, self.register_a & 0b0000_0001 != 0);
            self.register_a = self.register_a.wrapping_shr(1);
            self.update_zero_and_negative_flags(self.register_a);
        } else {
            let value = operand.get_data(self);
            self.status
                .set(StatusFlags::CARRY, value & 0b0000_0001 != 0);
            let result = value.wrapping_shr(1);
            self.bus.write(operand.data, result);
            self.update_zero_and_negative_flags(result);
        }
    }

    fn sbc(&mut self, operand: &Operand) {
        let value = operand.get_data(self);

        let borrow = !self.status.contains(StatusFlags::CARRY);
        // TODO: Use `borrowing_sub` when stable.
        let (_, overflow) = (self.register_a as i8).sub_borrowing(value as i8, borrow);
        let (result, borrow) = self.register_a.sub_borrowing(value, borrow);

        self.register_a = result;
        self.status.set(StatusFlags::CARRY, !borrow);
        self.status.set(StatusFlags::OVERFLOW, overflow);
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Add memory to accumulator with carry
    fn adc(&mut self, operand: &Operand) {
        let value = operand.get_data(self);

        let carry = self.status.contains(StatusFlags::CARRY);
        // TODO: Use `carrying_add` when stable.
        let (_, overflow) = (self.register_a as i8).add_carrying(value as i8, carry);
        let (result, carry) = self.register_a.add_carrying(value, carry);

        self.register_a = result;
        self.status.set(StatusFlags::CARRY, carry);
        self.status.set(StatusFlags::OVERFLOW, overflow);
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// "Exclusive-Or" memory with accumulator
    fn eor(&mut self, operand: &Operand) {
        self.register_a = self.register_a ^ operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ora(&mut self, operand: &Operand) {
        self.register_a = self.register_a | operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn and(&mut self, operand: &Operand) {
        self.register_a = self.register_a & operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rts(&mut self, _operand: &Operand) {
        self.program_counter = self.stack_pop_u16() + 1;
    }

    fn jsr(&mut self, operand: &Operand) {
        self.stack_push_u16(self.program_counter - 1);
        self.program_counter = operand.data;
    }

    /// Store accumulator in memory
    fn sta(&mut self, operand: &Operand) {
        self.bus.write(operand.data, self.register_a);
    }

    /// Store index X in memory
    fn stx(&mut self, operand: &Operand) {
        self.bus.write(operand.data, self.register_x);
    }

    /// Store index Y in memory
    fn sty(&mut self, operand: &Operand) {
        self.bus.write(operand.data, self.register_y);
    }

    fn ldy(&mut self, operand: &Operand) {
        self.register_y = operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Load index X with memory
    fn ldx(&mut self, operand: &Operand) {
        self.register_x = operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Load accumulator with memory
    fn lda(&mut self, operand: &Operand) {
        self.register_a = operand.get_data(self);
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Pull accumulator from stack
    fn pla(&mut self, _operand: &Operand) {
        self.register_a = self.stack_pop_u8();
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// PusH Accumulator
    fn pha(&mut self, _operand: &Operand) {
        self.stack_push_u8(self.register_a);
    }

    /// Pull processor status from stack
    fn plp(&mut self, _operand: &Operand) {
        let mut status = StatusFlags::from_bits_truncate(self.stack_pop_u8());
        status -= StatusFlags::BREAK_COMMAND;
        status |= StatusFlags::UNUSED;
        self.status = status;
    }

    /// PusH Processor status
    fn php(&mut self, _operand: &Operand) {
        let result = self.status.bits() | StatusFlags::BREAK_COMMAND.bits();
        self.stack_push_u8(result);
    }

    /// Transfer X to Stack ptr
    fn txs(&mut self, _operand: &Operand) {
        self.stack_pointer = self.register_x;
    }

    /// Transfer Stack ptr to X
    fn tsx(&mut self, _operand: &Operand) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self, _operand: &Operand) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tya(&mut self, _operand: &Operand) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Load accumulator and X register with memory
    fn lax(&mut self, operand: &Operand) {
        self.lda(operand);
        self.tax(operand);
    }

    fn sax(&mut self, operand: &Operand) {
        self.bus
            .write(operand.data, self.register_a & self.register_x);
    }

    fn dcp(&mut self, operand: &Operand) {
        self.dec(operand);
        self.cmp(operand);
    }

    fn isb(&mut self, operand: &Operand) {
        self.inc(operand);
        self.sbc(operand);
    }

    fn slo(&mut self, operand: &Operand) {
        self.asl(operand);
        self.ora(operand);
    }

    fn rla(&mut self, operand: &Operand) {
        self.rol(operand);
        self.and(operand);
    }

    fn sre(&mut self, operand: &Operand) {
        self.lsr(operand);
        self.eor(operand);
    }

    fn rra(&mut self, operand: &Operand) {
        self.ror(operand);
        self.adc(operand);
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
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0x05, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.contains(StatusFlags::ZERO));
        assert!(!cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0x00, 0x00]);
        cpu.reset();
        cpu.run();

        assert!(cpu.status.contains(StatusFlags::ZERO));
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0x0a, 0xaa, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa2, 0xff, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa5, 0x10, 0x00]);
        cpu.bus.write(0x10, 0x55);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_subroutines_with_inx() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x60,
        ]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 3);
    }

    #[test]
    fn test_nested_subroutines() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![
            0xa2, 0x01, 0x20, 0x09, 0x06, 0x20, 0x09, 0x06, 0x00, 0xe8, 0x20, 0x0f, 0x06, 0x60,
            0x00, 0xe8, 0x60, 0x00,
        ]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 5);
    }

    #[test]
    fn test_and() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0x10, 0x29, 0x30]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0x10);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_and_zero() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0x1, 0x29, 0x2]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 0);
        assert!(!cpu.status.is_empty());
    }

    #[test]
    fn test_adc() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa9, 0xff, 0x69, 0x1, 0x69, 0x1]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_a, 2);
        assert_eq!(cpu.status.bits(), 0b0011_0000);
    }

    #[test]
    fn test_dex_bpl() {
        let bus = Bus::new(&test_rom());
        let mut cpu = CPU::new(bus);

        cpu.load(vec![0xa2, 0x4, 0xca, 0xca, 0x10, 0xfd]);
        cpu.reset();
        cpu.run();

        assert_eq!(cpu.register_x, 0xff);
        assert!(cpu.status.contains(StatusFlags::NEGATIVE));
    }

    #[test]
    fn nestest() {
        use pretty_assertions::assert_eq;

        let nestest = include_bytes!("../tests/nestest.nes");
        let rom = Rom::new(&nestest.to_vec()).unwrap();
        let bus = Bus::new(&rom);
        let mut cpu = CPU::new(bus);
        cpu.reset();

        // Run in "automated" mode.
        cpu.program_counter = 0xc000;

        let mut expected = include_str!("../tests/nestest.log").lines();
        cpu.run_with_callback(|debug| {
            println!("{debug}");
            if let Some(line) = expected.next() {
                assert_eq!(debug, line);
            }
        });
    }
}
