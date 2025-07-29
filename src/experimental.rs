//! Experimental new version of file-rotate.

use std::{
    fs::File,
    io::{self, Write},
};

mod triggers;
mod rotators;

/// Handles *how* to rotate.
pub trait Rotator {
    /// Takes the previous file (None if the initial file), and returns a new file.
    ///
    /// Only runs when `Trigger` returns [Action::Rotate].
    fn rotate(&mut self, current: Option<File>) -> io::Result<File>;
}

/// Handles *when* to rotate.
pub trait Trigger {
    /// Called for every [write]. Counts properties of the output stream and decides whether to perform a rotation or not.
    fn trigger(&mut self, bytes: &[u8]) -> Action;
}

/// Decides whether to trigger a log rotation.
pub enum Action {
    /// Rotate the log file, reporting how many bytes the current log has consumed from the
    /// buffer to write to the log file. The remaining bytes will be written to the new log.
    Rotate {
        /// Amount of bytes that were written from the buffer to the file before rotation.
        consumed: usize
    },
    /// Do not perform a log rotation.
    None,
}

/// The main writer used for rotating logs.
pub struct FileRotate<R, T>
where
    R: Rotator,
    T: Trigger,
{
    file: Option<File>,
    rotator: R,
    trigger: T,
}

impl<R, T> FileRotate<R, T>
where R: Rotator,
      T: Trigger,
{
    fn new(mut rotator: R, trigger: T) -> io::Result<Self>
    {
        let file = Some(rotator.rotate(None)?);
        Ok(Self {
            file,
            rotator,
            trigger,
        })
    }
}

impl<R, T> Write for FileRotate<R, T>
where R: Rotator, T: Trigger, {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let mut begin = 0;
        loop {
            match self.trigger.trigger(&buf[begin..]) {
                Action::Rotate { consumed } => {
                    if let Some(file) = self.file.as_mut() {
                        file.write_all(&buf[begin..begin + consumed])?;
                    }
                    self.file = Some(self.rotator.rotate(self.file.take())?);
                    begin += consumed;
                }
                Action::None => {
                    if let Some(file) = self.file.as_mut() {
                        file.write_all(&buf[begin..])?;
                        return Ok(buf.len());
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "File missing during write",
                        ));
                    }
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        if let Some(file) = self.file.as_mut() {
            file.flush()
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "File missing during flush",
            ));
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

    write!(fr, "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*[]_+").unwrap();

    assert_eq!("EFGHIJKLMN", fs::read_to_string("foo.0").unwrap());
    assert_eq!("OPQRSTUVWX", fs::read_to_string("foo.1").unwrap());
    assert_eq!("YZ!@#$%^&*", fs::read_to_string("foo.2").unwrap());
    assert_eq!("[]_+", fs::read_to_string("foo.3").unwrap());
}
