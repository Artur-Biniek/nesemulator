use std::fs::File;
use std::io::prelude::*;

use super::mappers::Mapper;
use super::mappers::MapperZero;

const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_BANK_SIZE: usize = 0x4000;
const CHR_BANK_SIZE: usize = 0x2000;

#[derive(Debug, PartialEq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct Cartridge {
    mapper: Box<dyn Mapper>,
    pub mirroring: Mirroring,
}

impl Cartridge {
    pub fn new(path: &str) -> Result<Self, String> {
        if let Ok(ref mut file) = File::open(path) {
            let mut raw = Vec::new();

            if let Ok(_) = file.read_to_end(&mut raw) {
                if &raw[0..4] != NES_TAG {
                    return Err("File is not in iNES file format".to_string());
                }

                let ines_ver = (raw[7] >> 2) & 0b11;
                if ines_ver != 0 {
                    return Err("NES2.0 is not supported".to_string());
                }

                let four_screen = raw[6] & 0b1000 != 0;
                let vertical_mirroring = raw[6] & 0b1 != 0;
                let mirroring = match (four_screen, vertical_mirroring) {
                    (true, _) => Mirroring::FourScreen,
                    (false, true) => Mirroring::Vertical,
                    (false, false) => Mirroring::Horizontal,
                };

                let prg_rom_size = raw[4] as usize * PRG_BANK_SIZE;
                let chr_rom_size = raw[5] as usize * CHR_BANK_SIZE;
                let skip_trainer = raw[6] & 0b100 != 0;

                let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
                let chr_rom_start = prg_rom_start + prg_rom_size;

                let prg_rom = raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec();
                let chr_rom = raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec();

                let mapper = match (raw[7] & 0b1111_0000) | (raw[6] >> 4) {
                    0 => MapperZero::new(prg_rom, chr_rom),
                    m => return Err(format!("Mapper {} is not supported", m)),
                };

                return Ok(Cartridge {
                    mapper: Box::new(mapper),
                    mirroring,
                });
            }
        }

        Err(String::from("Couldn't read from rom file"))
    }

    pub fn read_cpu(&self, addr: u16) -> Option<u8> {
        self.mapper.read_cpu(addr)
    }

    pub fn read_ppu(&self, addr: u16) -> Option<u8> {
        self.mapper.read_ppu(addr)
    }
}
