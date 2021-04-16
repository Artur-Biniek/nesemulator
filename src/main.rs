mod nes;

use nes::Cartridge;
use nes::Console;

//use sdl2::event::Event;
//use sdl2::keyboard::Keycode;
//use sdl2::pixels::Color;
//use sdl2::pixels::PixelFormatEnum;
//use sdl2::rect::Rect;
//use std::time::Duration;

fn test() {
    let cartridge = Cartridge::new("nestest.nes").unwrap();

    let mut console = Console::new(cartridge);
    console.reset(Some(0xc000));
    //console.reset(None);
    loop {
        console.clock();
    }
}

// fn render(canvas: &mut WindowCanvas, color: Color) {
//     canvas.set_draw_color(color);
//     canvas.clear();
//     canvas.present();
// }

fn main() -> Result<(), String> {
    test();

    return Ok(());

    //    let sdl_context = sdl2::init()?;
    //    let video_subsystem = sdl_context.video()?;
    //
    //    let window = video_subsystem
    //        .window("nes", 800, 600)
    //        .position_centered()
    //        .build()
    //        .expect("could not initialize video subsystem");
    //
    //    let mut canvas = window
    //        .into_canvas()
    //        .build()
    //        .expect("could not make a canvas");
    //
    //    let creator = canvas.texture_creator();
    //    let mut texture = creator
    //        .create_texture_target(PixelFormatEnum::RGB24, 128, 128)
    //        .unwrap();
    //
    //    //let cart = Cartridge::new("nestest.nes").unwrap();
    //    let cart = Cartridge::new("smb.nes").unwrap();
    //
    //    let mut screen_state = [155 as u8; 128 * 128 * 3];
    //
    //    let pallete = [(0, 0, 0), (250, 0, 0), (0, 255, 0), (0, 0, 255)];
    //
    //    for tile_y in 0..16 {
    //        for tile_x in 0..16 {
    //            for fine_y in 0..8 {
    //                let addr = 0x0000 | tile_y << 8 | tile_x << 4 | fine_y;
    //
    //                let mut lsb = cart.read_ppu(addr).unwrap();
    //                let mut msb = cart.read_ppu(addr + 8).unwrap();
    //
    //                let mask = 0b1000_0000;
    //
    //                for fine_x in 0..8 {
    //                    let ind = (msb & mask) >> 6 | (lsb & mask) >> 7;
    //                    msb <<= 1;
    //                    lsb <<= 1;
    //
    //                    let (r, g, b) = pallete[ind as usize];
    //
    //                    let off =
    //                        ((tile_y * 8 + fine_y) * 128 * 3 + tile_x * 8 * 3 + fine_x * 3) as usize;
    //                    screen_state[off + 0] = r;
    //                    screen_state[off + 1] = g;
    //                    screen_state[off + 2] = b;
    //                }
    //            }
    //        }
    //    }
    //
    //    // for x in 0..128 {
    //    //     for y in 0..128 {
    //    //         if (x != y) {
    //    //             continue;
    //    //         };
    //
    //    //         let cord = y * 128 * 3 + x * 3;
    //    //         let (r, g, b) = pallete[ind];
    //
    //    //         screen_state[cord + 0] = r;
    //    //         screen_state[cord + 1] = g;
    //    //         screen_state[cord + 2] = b;
    //
    //    //         ind = ind + 1 & 0b0011;
    //    //     }
    //    // }
    //
    //    texture.update(None, &screen_state[..], 3 * 128).unwrap();
    //
    //    let mut event_pump = sdl_context.event_pump()?;
    //    let mut i = 0;
    //    'running: loop {
    //        for event in event_pump.poll_iter() {
    //            match event {
    //                Event::Quit { .. }
    //                | Event::KeyDown {
    //                    keycode: Some(Keycode::Escape),
    //                    ..
    //                } => {
    //                    break 'running;
    //                }
    //                _ => {}
    //            }
    //        }
    //
    //        // Update
    //        i = (i + 1) % 255;
    //
    //        // Render
    //        //render(&mut canvas, );
    //
    //        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
    //        canvas.clear();
    //        canvas
    //            .copy(
    //                &texture,
    //                Rect::new(0, 0, 128, 128),
    //                Rect::new(10, 10, 4 * 128, 4 * 128),
    //            )
    //            .unwrap();
    //        canvas.present();
    //
    //        // Time management!
    //        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    //    }
    //
    //    Ok(())
}
