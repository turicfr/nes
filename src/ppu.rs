use crate::rom::{Mirroring, Rom};
use bitflags::bitflags;

const CHR_ROM: u16 = 0x0;
const CHR_ROM_END: u16 = 0x1fff;
const RAM: u16 = 0x2000;
const RAM_END: u16 = 0x2fff;
const UNUSED: u16 = 0x3000;
const UNUSED_END: u16 = 0x3eff;
const PALETTE: u16 = 0x3f00;
const PALETTE_END: u16 = 0x3fff;

// https://www.nesdev.org/wiki/PPU_registers
const CTRL_REGISTER: u16 = 0x2000;
const MASK_REGISTER: u16 = 0x2001;
const STATUS_REGISTER: u16 = 0x2002;
const OAM_ADDR: u16 = 0x2003;
const OAM_DATA: u16 = 0x2004;
const SCROLL_REGISTER: u16 = 0x2005;
const ADDR_REGISTER: u16 = 0x2006;
const DATA_REGISTER: u16 = 0x2007;
const OAM_DMA: u16 = 0x4014;

trait SetBytes {
    fn set_low(&mut self, value: u8);
    fn set_high(&mut self, value: u8);
}

impl SetBytes for u16 {
    fn set_low(&mut self, value: u8) {
        *self &= !0x00ff;
        *self |= value as u16;
    }

    fn set_high(&mut self, value: u8) {
        *self &= !0xff00;
        *self |= (value as u16) << 8;
    }
}

// https://www.nesdev.org/wiki/PPU_memory_map
//
//  _______________  $4000  _______________
// |    Mirrors    |       |               |
// |  $3F00-$3F1F  |       |               |
// |_ _ _ _ _ _ _ _| $3F20 |  Palette RAM  |
// |  Palette RAM  |       |    indexes    |
// |    indexes    |       |               |
// |_______________| $3F00 |_______________|
// |    Unused     |       |    Unused     |
// |_______________| $3000 |_______________|
// |  Nametable 3  |       |               |
// |_ _ _ _ _ _ _ _| $2C00 |               |
// |  Nametable 2  |       |               |
// |_ _ _ _ _ _ _ _| $2800 |     VRAM      |
// |  Nametable 1  |       |               |
// |_ _ _ _ _ _ _ _| $2400 |               |
// |  Nametable 0  |       |               |
// |_______________| $2000 |_______________|
// |               |       |               |
// |Pattern table 1|       |               |
// |               |       | Pattern table |
// |_ _ _ _ _ _ _ _| $1000 |   (CHR ROM)   |
// |               |       |               |
// |Pattern table 0|       |               |
// |               |       |               |
// |_______________| $0000 |_______________|

pub struct PPU {
    chr_rom: Vec<u8>,
    palette_table: [u8; 32],
    name_table: [u8; 0x1000],
    _oam_data: [u8; 0x100],
    internal_data_buf: u8,
    mirroring: Mirroring,
    w_register: bool,
    addr: u16,
    scroll: u16,
    ctrl: ControlRegister,
    status: StatusRegister,
    mask: MaskRegister,
    scanline: u16,
    cycles: usize,
    pub nmi: bool,
}

impl PPU {
    pub fn new(rom: &Rom) -> Self {
        Self {
            chr_rom: rom.chr_rom.clone(),
            palette_table: [0; 32],
            name_table: [0; 0x1000],
            _oam_data: [0; 64 * 4],
            internal_data_buf: 0,
            mirroring: rom.screen_mirroring,
            w_register: false,
            addr: 0,
            scroll: 0,
            ctrl: ControlRegister::empty(),
            status: StatusRegister::from_bits_truncate(0b1000_0000),
            mask: MaskRegister::empty(),
            scanline: 0,
            cycles: 0,
            nmi: false,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            self.cycles = 0;
            self.scanline += 1;

            if self.scanline == 241 {
                if self.ctrl.contains(ControlRegister::GENERATE_NMI) {
                    self.nmi = true;
                    self.status.insert(StatusRegister::VBLANK_STARTED);
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.status.remove(StatusRegister::VBLANK_STARTED);
                return true;
            }
        }
        return false;
    }

    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        match addr {
            STATUS_REGISTER => {
                self.status.insert(StatusRegister::VBLANK_STARTED); // TODO: get rid of this.
                self.w_register = false;
                self.status.bits()
            }
            OAM_DATA => {
                todo!("read OAM_DATA")
            }
            DATA_REGISTER => self.read_data(),
            _ => panic!("Attempt to read from write-only PPU address {addr:#x}"),
        }
    }

    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        match addr {
            CTRL_REGISTER => self.ctrl = ControlRegister::from_bits_truncate(data),
            MASK_REGISTER => self.mask = MaskRegister::from_bits_truncate(data),
            OAM_ADDR => {
                todo!("write OAM_ADDR")
            }
            OAM_DATA => {
                todo!("write OAM_DATA")
            }
            SCROLL_REGISTER => {
                if self.w_register {
                    self.scroll.set_low(data);
                } else {
                    self.scroll.set_high(data);
                }

                self.w_register = !self.w_register;
            }
            ADDR_REGISTER => {
                if self.w_register {
                    self.addr.set_low(data);
                } else {
                    self.addr.set_high(data);
                }
                self.addr = self.addr & 0x3fff;

                self.w_register = !self.w_register;
            }
            DATA_REGISTER => self.write_data(data),
            OAM_DMA => {
                todo!("write OAM_DMA")
            }
            _ => panic!("Attempt to write at read-only PPU address {addr:#x}"),
        }
    }

    /// Reads data from the PPU memory.
    fn read_data(&mut self) -> u8 {
        let addr = self.addr;
        self.addr = self.addr.wrapping_add(self.ctrl.vram_addr_increment()) & 0x3fff;

        match addr {
            CHR_ROM..=CHR_ROM_END => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            RAM..=RAM_END => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.name_table[self.mirror_vram_addr(addr) as usize];
                result
            }
            UNUSED..=UNUSED_END => {
                panic!(
                    "address space {UNUSED:#x}..{UNUSED_END:#x} is not expected to be used, requested {addr:#x}"
                )
            }
            PALETTE..=PALETTE_END => self.palette_table[(addr - PALETTE) as usize],
            _ => panic!("unexpected access to mirrored address space {addr:#x}"),
        }
    }

    /// Writes data to the PPU memory.
    fn write_data(&mut self, data: u8) {
        let addr = self.addr;
        self.addr = self.addr.wrapping_add(self.ctrl.vram_addr_increment()) & 0x3fff;

        match addr {
            CHR_ROM..=CHR_ROM_END => {
                panic!("attempt to write to read-only PPU CHR_ROM at {addr:#x}")
            }
            RAM..=RAM_END => self.name_table[(addr - RAM) as usize] = data,
            UNUSED..=UNUSED_END => {
                panic!(
                    "address space {UNUSED:#x}..{UNUSED_END:#x} is not expected to be used, requested {addr:#x}"
                )
            }
            PALETTE..=PALETTE_END => {
                let index = (addr - PALETTE) % 32;
                self.palette_table[index as usize] = data;
            }
            _ => panic!("unexpected access to mirrored address space {addr:#x}"),
        }
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let vram_index = (addr & 0x2fff) - RAM;
        let name_table = vram_index / 0x400;
        match (&self.mirroring, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }
}

bitflags! {
    struct ControlRegister: u8 {
        /// Base nametable address.
        const NAMETABLE1              = 1 << 0;
        /// Base nametable address.
        const NAMETABLE2              = 1 << 1;
        /// VRAM address increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down).
        const VRAM_ADD_INCREMENT      = 1 << 2;
        /// Sprite pattern table address for 8x8 sprites.
        const SPRITE_PATTERN_ADDR     = 1 << 3;
        /// Background pattern table address (0: $0000; 1: $1000).
        const BACKROUND_PATTERN_ADDR  = 1 << 4;
        /// Sprite size (0: 8x8 pixels; 1: 8x16 pixels).
        const SPRITE_SIZE             = 1 << 5;
        /// PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins).
        const MASTER_SLAVE_SELECT     = 1 << 6;
        /// Generate an NMI at the start of the vertical blanking interval (0: off; 1: on).
        const GENERATE_NMI            = 1 << 7;
    }
}

impl ControlRegister {
    fn vram_addr_increment(&self) -> u16 {
        if self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            32
        } else {
            1
        }
    }
}

bitflags! {
    struct StatusRegister: u8 {
        /// PPU open bus. Returns stale PPU bus contents.
        const OPEN_BUS        = 0b0001_1111;
        /// The intent was for this flag to be set whenever more than eight sprites appear on a
        /// scanline, but a hardware bug causes the actual behavior to be more complicated and
        /// generate false positives as well as false negatives.
        /// This flag is set during sprite evaluation and cleared at dot 1 (the second dot) of the
        /// pre-render line.
        const SPRITE_OVERFLOW = 1 << 5;
        /// Set when a nonzero pixel of sprite 0 overlaps a nonzero background pixel;
        /// cleared at dot 1 of the pre-render line. Used for raster timing.
        const SPRITE_0_HIT    = 1 << 6;
        /// Vertical blank has started (0: not in vblank; 1: in vblank).
        /// Set at dot 1 of line 241 (the line *after* the post-render line); cleared after reading
        /// $2002 and at dot 1 of the pre-render line.
        const VBLANK_STARTED  = 1 << 7;
    }
}

bitflags! {
    struct MaskRegister: u8 {
        /// Greyscale (0: normal color, 1: produce a greyscale display).
        const GREYSCALE                = 1 << 0;
        /// 1: Show background in leftmost 8 pixels of screen, 0: hide
        const SHOW_BACKGROUND_LEFTMOST = 1 << 1;
        /// 1: Show sprites in leftmost 8 pixels of screen, 0: hide
        const SHOW_SPRITES_LEFTMOST    = 1 << 2;
        const SHOW_BACKGROUND          = 1 << 3;
        const SHOW_SPRITES             = 1 << 4;
        const EMPHASIZE_RED            = 1 << 5;
        const EMPHASIZE_GREEN          = 1 << 6;
        const EMPHASIZE_BLUE           = 1 << 7;
    }
}
