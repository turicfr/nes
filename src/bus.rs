use crate::Rom;

const RAM: u16 = 0x0;
const RAM_MIRRORS_END: u16 = 0x1fff;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3fff;
const PRG_ROM: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xffff;

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
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Self {
            cpu_vram: [0u8; 0x800],
            rom,
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => self.cpu_vram[(addr & 0b0111_1111_1111) as usize],
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = dbg!(addr & 0b00100000_00000111);
                todo!("PPU is not supported yet");
            }
            PRG_ROM..=PRG_ROM_END => self.rom.mem_read(addr - PRG_ROM),
            _ => {
                println!("Ignoring mem read access at {:x}", addr);
                0
            }
        }
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => self.cpu_vram[(addr & 0b0111_1111_1111) as usize] = data,
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet");
            }
            PRG_ROM..=PRG_ROM_END => panic!("Attempt to write to cartridge ROM space"),
            _ => println!("Ignoring mem write access at {:x}", addr),
        }
    }
}
