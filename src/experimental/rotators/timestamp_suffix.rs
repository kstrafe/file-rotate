use super::*;
use chrono::{format::ParseErrorKind, offset::Local, prelude::*, Duration, NaiveDateTime};

pub struct TimestampSuffix {
    base: &'static str,
    format: &'static str,
}

impl TimestampSuffix {
    pub fn new(base: &'static str) -> Self {
        Self { base, format: "" }
    }

    pub fn format(mut self, format: &'static str) -> Self {
        self.format = format;
        self
    }
}

impl Rotator for TimestampSuffix {
    fn rotate(&mut self, file: Option<File>) -> io::Result<File> {
        if file.is_none() {
            return File::create(self.base);
        }

        File::create(format!("{}.{}", self.base, self.format))
    }
}
