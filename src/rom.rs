use bincode::deserialize;
use bitflags::bitflags;
use serde::Deserialize;

const NES_TAG: [u8; 4] = [b'N', b'E', b'S', 0x1a];
const PRG_ROM_PAGE_SIZE: usize = 0x4000;
const CHR_ROM_PAGE_SIZE: usize = 0x2000;
const TRAINER_SIZE: usize = 0x200;

bitflags! {
    #[derive(Deserialize, Debug)]
    struct Flags6: u8 {
        const NAMETABLE_ARRANGEMENT = 1 << 0;
        const PRG_RAM_BATTERY       = 1 << 1;
        const TRAINER               = 1 << 2;
        const ALTERNATIVE_NAMETABLE = 1 << 3;
        const LO_MAPPER             = 0b1111_0000;
    }

    #[derive(Deserialize, Debug)]
    struct Flags7: u8 {
        const VS_UNISYSTEM  = 1 << 0;
        const PLAYCHOICE_10 = 1 << 1;
        const VERSION       = 0b0000_1100;
        const HI_MAPPER     = 0b1111_0000;
    }
}

#[derive(Deserialize, Debug)]
struct Header {
    format: [u8; 4],
    prg_rom_banks: u8,
    chr_rom_banks: u8,
    flags6: Flags6,
    flags7: Flags7,
    _flags8: u8,
    _flags9: u8,
    _unused: [u8; 6],
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    _mapper: u8,
    pub screen_mirroring: Mirroring,
}

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Self, String> {
        let header: Header = deserialize(&raw[..16]).unwrap();

        if header.format != NES_TAG {
            return Err("File is not in iNES file format".to_string());
        }

        let four_screen = header.flags6.contains(Flags6::ALTERNATIVE_NAMETABLE);
        let vertical_mirroring = header.flags6.contains(Flags6::NAMETABLE_ARRANGEMENT);
        let screen_mirroring = match (four_screen, vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let mapper = (header.flags7.bits() & Flags7::HI_MAPPER.bits()) << 4
            | header.flags6.bits() & Flags6::LO_MAPPER.bits();
        if (header.flags7 & Flags7::VERSION).bits() != 0 {
            return Err("NES 2.0 format is not supported".to_string());
        }

        let has_trainer = header.flags6.contains(Flags6::TRAINER);
        let prg_rom_start: usize = 16 + if has_trainer { TRAINER_SIZE } else { 0 };
        let chr_rom_start = prg_rom_start + header.prg_rom_banks as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_end = chr_rom_start + header.chr_rom_banks as usize * CHR_ROM_PAGE_SIZE;

        Ok(Rom {
            prg_rom: raw[prg_rom_start..chr_rom_start].to_vec(),
            chr_rom: raw[chr_rom_start..chr_rom_end].to_vec(),
            _mapper: mapper,
            screen_mirroring,
        })
    }
}

#[cfg(test)]
pub mod tests {
    use crate::rom::{Mirroring, CHR_ROM_PAGE_SIZE, PRG_ROM_PAGE_SIZE};
    use crate::Rom;

    pub fn test_rom(data: Vec<u8>) -> Rom {
        let mut prg_rom = vec![0u8; PRG_ROM_PAGE_SIZE * 2];
        prg_rom.splice(0x600..0x600 + data.len(), data);
        prg_rom[0x7ffc] = 0x0;
        prg_rom[0x7ffd] = 0x86;

        Rom {
            prg_rom,
            chr_rom: Vec::with_capacity(CHR_ROM_PAGE_SIZE),
            _mapper: 0,
            screen_mirroring: Mirroring::Vertical,
        }
    }
}
