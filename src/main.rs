mod nes;

use nes::cpu::Cpu;
use nes::bus::Bus;

fn main() {
    let mut cpu = Cpu::new();
    let mut bus = Bus::new(vec![0x00]);
    cpu.step(&mut bus);
    println!("Hello, world!");
}
