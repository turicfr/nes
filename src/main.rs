use crate::rom::Rom;
use bus::Bus;
use cpu::CPU;
use rand::{thread_rng, Rng};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::EventPump;
use std::fs::File;
use std::io::Read;

mod bus;
mod cpu;
mod rom;
mod carrying;

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

fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => cpu.write_u8(0xff, 0x77),
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => cpu.write_u8(0xff, 0x61),
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => cpu.write_u8(0xff, 0x73),
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => cpu.write_u8(0xff, 0x64),
            _ => {}
        }
    }
}

fn read_screen_state(cpu: &mut CPU, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200..0x600 {
        let (b1, b2, b3) = color(cpu.peek_u8(i)).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn read_file(filename: &String) -> Vec<u8> {
    let mut f = File::open(&filename).expect("ROM file not found");
    let metadata = std::fs::metadata(&filename).expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    f.read(&mut buffer).expect("buffer overflow");

    buffer
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    let game = read_file(&args[1]);

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("NES Emulator", (32.0 * 20.0) as u32, (32.0 * 20.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(20.0, 20.0).unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();

    let rom = Rom::new(&game).unwrap();
    let bus = Bus::new(rom);
    let mut cpu = CPU::new(bus);
    cpu.reset();

    let mut screen_state = [0u8; 32 * 32 * 3];
    let mut rng = thread_rng();

    cpu.run_with_callback(move |cpu| {
        handle_user_input(cpu, &mut event_pump);
        cpu.write_u8(0xfe, rng.gen_range(1..16));
        if read_screen_state(cpu, &mut screen_state) {
            texture.update(None, &screen_state, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        std::thread::sleep(std::time::Duration::new(0, 70_000));
    });
}
