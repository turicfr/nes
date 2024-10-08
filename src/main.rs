use crate::rom::Rom;
use bus::Bus;
use cpu::CPU;
use render::frame::Frame;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};

mod bus;
mod carrying;
mod cpu;
mod ppu;
mod render;
mod rom;

#[allow(dead_code)]
fn color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

const WIDTH: usize = 256;
const HEIGHT: usize = 240;

fn show_tiles(chr_rom: &[u8], bank: usize) -> Frame {
    assert!(bank <= 1);

    let mut frame = Frame::new();
    let bank = bank * 0x1000;
    let mut offset_x = 0;
    let mut offset_y = 0;

    for i in 0..256 {
        if i % 32 == 0 {
            offset_x = 0;
            offset_y += 8;
        }
        let start = bank + i * 16;
        let plane_0 = &chr_rom[start..start + 8];
        let plane_1 = &chr_rom[start + 8..start + 16];

        for (y, (a, b)) in plane_0.iter().zip(plane_1).enumerate() {
            for x in (0..8).rev() {
                let bit_a = (a >> x as u8) & 1;
                let bit_b = (b >> x as u8) & 1;

                let value = bit_b << 1 | bit_a;
                let abgr = match value {
                    0 => [0, 0, 0, 0],
                    1 => [0xff, 0x00, 0x3D, 0xA6],
                    2 => [0xff, 0xDA, 0xAB, 0xEB],
                    3 => [0xff, 0x0C, 0xF0, 0xA4],
                    _ => unreachable!(),
                };

                frame.set_pixel((7 - x) + offset_x, y + offset_y, abgr);
            }
        }
        offset_x += 8;
    }

    frame
}

fn main() -> Result<(), String> {
    let args: Vec<_> = std::env::args().collect();
    let game = std::fs::read(&args[1]).unwrap();

    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let window = video_subsystem
        .window("NES Emulator", WIDTH as u32 * 3, HEIGHT as u32 * 3)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(|e| e.to_string())?;
    canvas.set_scale(10.0, 10.0)?;

    let texture_creator = canvas.texture_creator();

    let mut texture = texture_creator
        .create_texture_target(PixelFormatEnum::RGBA8888, WIDTH as u32, HEIGHT as u32)
        .map_err(|e| e.to_string())?;

    let rom = Rom::new(&game)?;
    let bus = Bus::new(&rom);
    let mut cpu = CPU::new(bus);
    cpu.reset();

    let mut event_pump = sdl_context.event_pump()?;
    cpu.run_with_callback(move |_debug| {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                _ => {}
            }
        }

        let frame = &show_tiles(&rom.chr_rom, 1);
        texture.update(None, &frame.data, Frame::WIDTH * 4).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        std::thread::sleep(std::time::Duration::new(0, 70_000));
    });

    Ok(())
}
