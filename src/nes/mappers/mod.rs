pub trait Mapper {
    fn read_cpu(&self, address: u16) -> Option<u8>;
    fn read_ppu(&self, address: u16) -> Option<u8>;
}

mod mapper_zero;

pub use mapper_zero::MapperZero;
