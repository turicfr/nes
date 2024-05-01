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
// |   Mirrors     |       |               |
// |  $2000-$2007  |       | I/O Registers |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// |   Mirrors     |       |               |
// |  $0000-$07FF  |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// |      RAM      |       |               |
// |_ _ _ _ _ _ _ _| $0200 |      RAM      |
// |     Stack     |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// |   Zero Page   |       |               |
// |_______________| $0000 |_______________|

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub struct Bus {
    cpu_vram: [u8; 2048],
}

impl Bus {
    pub fn new() -> Self {
        Self {
            cpu_vram: [0; 2048],
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => self.cpu_vram[(addr & 0b0111_1111_1111) as usize],
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b00100000_00000111;
                todo!("PPU is not supported yet");
            }
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
            _ => println!("Ignoring mem write access at {:x}", addr),
        }
    }
}
