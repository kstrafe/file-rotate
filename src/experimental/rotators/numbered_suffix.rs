use super::*;
use std::{borrow::Cow, fs};

pub struct NumberedSuffix {
    base: Cow<'static, str>,
    current: usize,
    max: usize,
}

impl NumberedSuffix {
    pub fn new<B>(base: B) -> Self
    where
        B: Into<Cow<'static, str>>,
    {
        Self {
            base: base.into(),
            current: 0,
            max: 0,
        }
    }

    pub fn max(mut self, max: usize) -> Self {
        self.max = max;
        self
    }
}

impl Rotator for NumberedSuffix {
    fn rotate(&mut self, file: Option<File>) -> io::Result<File> {
        let Some(mut file) = file else {
            return File::create(&*self.base);
        };

        file.flush()?;
        let new_path = format!("{}.{}", self.base, self.current);
        fs::rename(&*self.base, &new_path)?;

        self.current += 1;
        self.current %= self.max + 1;

        let new_file = File::create(&*self.base);

        new_file
    }
}
