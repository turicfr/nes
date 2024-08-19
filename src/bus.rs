use crate::ppu::PPU;
use crate::Rom;

const RAM: u16 = 0x0;
const RAM_MIRRORS_END: u16 = 0x1fff;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;
const PRG_ROM: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xffff;

const PRG_ROM_PAGE_SIZE: usize = 0x4000;

// https://www.nesdev.org/wiki/CPU_memory_map
//
//  _______________ $10000  _______________
// |    PRG-ROM    |       |               |
// |   Upper Bank  |       |               |
// |_ _ _ _ _ _ _ _| $C000 |    PRG-ROM    |
// |    PRG-ROM    |       |               |
// |   Lower Bank  |       |               |
// |_______________| $8000 |_______________|
// |     SRAM      |       |     SRAM      |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// |    Mirrors    |       |               |
// |  $2000-$2007  |       | I/O Registers |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// |    Mirrors    |       |               |
// |  $0000-$07FF  |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// |      RAM      |       |               |
// |_ _ _ _ _ _ _ _| $0200 |      RAM      |
// |     Stack     |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// |   Zero Page   |       |               |
// |_______________| $0000 |_______________|

pub struct Bus {
    cpu_vram: [u8; 0x800],
    prg_rom: Vec<u8>,
    pub ppu: PPU,
    pub cycles: usize,
}

impl Bus {
    pub fn new(rom: &Rom) -> Self {
        Self {
            cpu_vram: [0u8; 0x800],
            prg_rom: rom.prg_rom.clone(),
            ppu: PPU::new(rom),
            cycles: 0,
        }
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        if self.ppu.nmi {
            self.ppu.nmi = false;
            Some(1)
        } else {
            None
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        self.cycles += cycles as usize;
        self.ppu.tick(cycles * 3);
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => self.cpu_vram[(addr & 0x7ff) as usize],
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => self.ppu.cpu_read(addr & 0x2007),
            PRG_ROM..=PRG_ROM_END => {
                let addr = addr - PRG_ROM;
                if self.prg_rom.len() == PRG_ROM_PAGE_SIZE && addr >= PRG_ROM_PAGE_SIZE as u16 {
                    // Mirror if needed
                    self.prg_rom[addr as usize % PRG_ROM_PAGE_SIZE]
                } else {
                    self.prg_rom[addr as usize]
                }
            }
            _ => {
                println!("Ignoring mem read access at {addr:#x}");
                0
            }
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => self.cpu_vram[(addr & 0x7ff) as usize] = data,
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => self.ppu.cpu_write(addr & 0x2007, data),
            PRG_ROM..=PRG_ROM_END => panic!("Attempt to write at {addr:#x} to cartridge ROM space"),
            _ => println!("Ignoring mem write access at {addr:#x}"),
        }
    }
}
