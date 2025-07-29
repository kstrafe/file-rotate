use super::*;

pub struct NumberedSuffix {
    current: usize,
    max: usize,
}

impl NumberedSuffix {
    pub fn new() -> Self {
        Self { current: 0, max: 0 }
    }

    pub fn max(mut self, max: usize) -> Self {
        self.max = max;
        self
    }
}

impl Rotator for NumberedSuffix {
    fn rotate(&mut self, file: Option<File>) -> io::Result<File> {
        if file.is_none() {
            return File::create("foo.0");
        }

        self.current += 1;
        self.current %= self.max + 1;
        File::create(format!("foo.{}", self.current))
    }
}
