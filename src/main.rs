mod nes;

use nes::Cartridge;
use nes::Console;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use std::time::Duration;

// fn render(canvas: &mut WindowCanvas, color: Color) {
//     canvas.set_draw_color(color);
//     canvas.clear();
//     canvas.present();
// }

fn main() -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let window = video_subsystem
        .window("nes", 800, 600)
        .position_centered()
        .build()
        .expect("could not initialize video subsystem");

    let mut canvas = window
        .into_canvas()
        .build()
        .expect("could not make a canvas");

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    canvas.present();

    //let cartridge = Cartridge::new("nestest.nes").unwrap();
    //let cartridge = Cartridge::new("smb.nes").unwrap();
   let cartridge = Cartridge::new("nestest.nes").unwrap();

    let mut console = Console::new(cartridge);
    //console.reset(Some(0xc000));
    console.reset(None);

    let mut event_pump = sdl_context.event_pump()?;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'running;
                }
                _ => {}
            }
        }

        let res = console.clock();
        if res {
            canvas.set_draw_color(Color::RGB(0, 0, 0));
            canvas.clear();

            texture.update(None, &console.video, 3 * 256).unwrap();

            canvas
                .copy(
                    &texture,
                    Rect::new(0, 0, 256, 240),
                    Rect::new(10, 10, 10 + 2 * 256, 10 + 2 * 240),
                )
                .unwrap();

            canvas.present();
        }

    }

    Ok(())
}
