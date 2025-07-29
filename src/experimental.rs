//! Experimental new version of file-rotate.

use std::{
    fs::File,
    io::{self, Write},
};

trait Rotator {
    /// Takes the previous file (None if the initial file), and returns a new file.
    ///
    /// Only runs when `Trigger` decides it's time to perform a rotation.
    fn rotate(&mut self, current: Option<File>) -> io::Result<File>;
}

trait Trigger {
    /// Counts properties of the output stream and decides whether to perform a rotation or not.
    fn trigger(&mut self, bytes: &[u8]) -> Action;
}

enum Action {
    Rotate { consumed: usize },
    None,
}

/// d
pub struct FileRotate {
    file: Option<File>,
    rotator: Box<dyn Rotator>,
    trigger: Box<dyn Trigger>,
}

impl FileRotate {
    fn new<R, T>(mut rotator: R, trigger: T) -> io::Result<Self>
    where
        R: Rotator + 'static,
        T: Trigger + 'static,
    {
        let file = Some(rotator.rotate(None)?);
        Ok(Self {
            file,
            rotator: Box::new(rotator),
            trigger: Box::new(trigger),
        })
    }
}

impl Write for FileRotate {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let mut begin = 0;
        loop {
            match self.trigger.trigger(&buf[begin..]) {
                Action::Rotate { consumed } => {
                    self.file
                        .as_mut()
                        .unwrap()
                        .write_all(&buf[begin..begin + consumed])?;
                    self.file = Some(self.rotator.rotate(self.file.take())?);
                    begin += consumed;
                }
                Action::None => {
                    self.file.as_mut().unwrap().write_all(&buf[begin..])?;
                    return Ok(buf.len());
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        let file = self.file.as_mut().unwrap();
        file.flush()
    }
}

mod triggers {
    use super::*;
    pub struct Bytes {
        pub count: usize,
        pub limit: usize,
    }

    impl Bytes {
        pub fn new() -> Self {
            let byte_count_1_MiB = 1048576;
            Self {
                count: 0,
                limit: byte_count_1_MiB,
            }
        }

        pub fn limit(mut self, bytes: usize) -> Self {
            self.limit = bytes;
            self
        }
    }

    impl Trigger for Bytes {
        fn trigger(&mut self, bytes: &[u8]) -> Action {
            if self.count + bytes.len() > self.limit {
                println!(
                    "consumed: self.limit={} self.count={}",
                    self.limit, self.count
                );
                let consumed = self.limit - self.count;
                self.count = 0;
                Action::Rotate { consumed }
            } else {
                self.count += bytes.len();
                Action::None
            }
        }
    }
}

mod rotators {
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
}

#[test]
fn basic() {
    use std::fs;
    let rotator = rotators::NumberedSuffix::new().max(3);
    let trigger = triggers::Bytes::new().limit(10);
    let mut fr = FileRotate::new(rotator, trigger).unwrap();

    write!(fr, "abcdefghijklmnopqrstuvwxyz0123456789").unwrap();

    assert_eq!("abcdefghij", fs::read_to_string("foo.0").unwrap());
    assert_eq!("klmnopqrst", fs::read_to_string("foo.1").unwrap());
    assert_eq!("uvwxyz0123", fs::read_to_string("foo.2").unwrap());
    assert_eq!("456789", fs::read_to_string("foo.3").unwrap());
}
