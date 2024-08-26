pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    pub const WIDTH: usize = 256;
    pub const HIGHT: usize = 240;

    pub fn new() -> Self {
        Self {
            data: vec![0; Self::WIDTH * Self::HIGHT * 4],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, abgr: [u8; 4]) {
        let base = y * 4 * Self::WIDTH + x * 4;
        self.data.splice(base..base + 4, abgr);
    }
}
