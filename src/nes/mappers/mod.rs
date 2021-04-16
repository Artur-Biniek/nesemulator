pub trait Mapper {
    fn read_cpu(&self, address: u16) -> Option<u8>;
    fn write_cpu(&self, addr: u16, value: u8) -> Option<()>;
    fn read_ppu(&self, address: u16) -> Option<u8>;
    fn write_ppu(&self, addr: u16, value: u8) -> Option<()>;
}

mod mapper_zero;

pub use mapper_zero::MapperZero;
