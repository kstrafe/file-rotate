//! Experimental new version of file-rotate with Write-based rotation.

use std::{
    fs::{self, File},
    io::{self, Write},
    borrow::Cow,
    mem,
};

/// Handles *when* to rotate.
pub trait Trigger {
    /// Called for every write. Counts properties of the output stream and decides whether to perform a rotation or not.
    fn trigger(&mut self, bytes: &[u8]) -> Action;
    /// Reset the current state. Called when `trigger` returns Action::Rotate.
    fn reset(&mut self);
}

/// Rotator provides Write instances and handles switching between them
pub trait Rotator {
    type Writer: Write;

    /// Create the initial writer
    fn initial(&mut self) -> io::Result<Self::Writer>;

    /// Rotate to a new writer, taking the previous one for cleanup
    fn rotate(&mut self, current: Self::Writer) -> io::Result<Self::Writer>;
}

/// Decides whether to trigger a log rotation.
pub enum Action {
    /// Rotate the log file, reporting how many bytes the current log has consumed from the
    /// buffer to write to the log file. The remaining bytes will be written to the new log.
    Rotate {
        /// Amount of bytes that were written from the buffer to the file before rotation.
        consumed: usize,
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
    writer: Option<R::Writer>,
    rotator: R,
    trigger: T,
}

impl<R, T> FileRotate<R, T>
where
    R: Rotator,
    T: Trigger,
{
    pub fn new(mut rotator: R, trigger: T) -> io::Result<Self> {
        let writer = Some(rotator.initial()?);
        Ok(Self { writer, rotator, trigger })
    }
}

impl<R, T> Write for FileRotate<R, T>
where
    R: Rotator,
    T: Trigger,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut begin = 0;

        loop {
            match self.trigger.trigger(&buf[begin..]) {
                Action::Rotate { consumed } => {
                    // Write consumed bytes to current writer
                    if let Some(writer) = &mut self.writer {
                        writer.write_all(&buf[begin..begin + consumed])?;
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "No writer available",
                        ));
                    }

                    // Take the current writer and rotate to a new one
                    let old_writer = self.writer.take().ok_or_else(|| {
                        io::Error::new(io::ErrorKind::Other, "No writer to rotate")
                    })?;

                    self.writer = Some(self.rotator.rotate(old_writer)?);

                    begin += consumed;
                    self.trigger.reset();
                }
                Action::None => {
                    if let Some(writer) = &mut self.writer {
                        writer.write_all(&buf[begin..])?;
                        return Ok(buf.len());
                    } else {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            "No writer available",
                        ));
                    }
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        if let Some(writer) = &mut self.writer {
            writer.flush()
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "No writer to flush",
            ))
        }
    }
}

/// Line count trigger - rotates after a specified number of lines
pub struct LineCount {
    limit: usize,
    current: usize,
}

impl LineCount {
    pub fn new() -> Self {
        Self { limit: 0, current: 0 }
    }

    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

impl Trigger for LineCount {
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        let newlines = bytes.iter().filter(|&&b| b == b'\n').count();

        if self.current + newlines >= self.limit {
            // Find the position of the line that would exceed the limit
            let mut lines_seen = 0;
            let mut consumed = 0;

            for (i, &byte) in bytes.iter().enumerate() {
                if byte == b'\n' {
                    lines_seen += 1;
                    if self.current + lines_seen >= self.limit {
                        consumed = i + 1; // Include the newline
                        break;
                    }
                }
            }

            Action::Rotate { consumed }
        } else {
            self.current += newlines;
            Action::None
        }
    }

    fn reset(&mut self) {
        self.current = 0;
    }
}

/// Trigger combinator that allows OR logic between triggers
pub struct TriggerCombinator<A, B> {
    trigger_a: A,
    trigger_b: B,
}

impl<A, B> TriggerCombinator<A, B>
where
    A: Trigger,
    B: Trigger,
{
    pub fn new(trigger_a: A, trigger_b: B) -> Self {
        Self { trigger_a, trigger_b }
    }
}

impl<A, B> Trigger for TriggerCombinator<A, B>
where
    A: Trigger,
    B: Trigger,
{
    fn trigger(&mut self, bytes: &[u8]) -> Action {
        // Check trigger A first
        match self.trigger_a.trigger(bytes) {
            Action::Rotate { consumed } => Action::Rotate { consumed },
            Action::None => {
                // Only check trigger B if A didn't trigger
                self.trigger_b.trigger(bytes)
            }
        }
    }

    fn reset(&mut self) {
        self.trigger_a.reset();
        self.trigger_b.reset();
    }
}

/// Extension trait to add .or() method to triggers
pub trait TriggerExt: Trigger + Sized {
    fn or<T: Trigger>(self, other: T) -> TriggerCombinator<Self, T> {
        TriggerCombinator::new(self, other)
    }
}

impl<T: Trigger> TriggerExt for T {}

pub mod rotators {
    use super::*;
    use std::path::PathBuf;

    /// File-based rotator using numbered suffixes
    pub struct NumberedSuffix {
        base_path: PathBuf,
        max_files: usize,
    }

    impl NumberedSuffix {
        pub fn new<B>(base: B) -> Self
        where
            B: Into<PathBuf>,
        {
            Self {
                base_path: base.into(),
                max_files: 5,
            }
        }

        pub fn max(mut self, max: usize) -> Self {
            self.max_files = max;
            self
        }
    }

    impl Rotator for NumberedSuffix {
        type Writer = File;

        fn initial(&mut self) -> io::Result<Self::Writer> {
            File::create(&self.base_path)
        }

        fn rotate(&mut self, mut current: Self::Writer) -> io::Result<Self::Writer> {
            current.flush()?;
            drop(current); // Close the file

            let mut last = 0;

            // Rotate existing backup files
            for i in 0..self.max_files {
                let from = self.base_path.with_extension(format!("{}", i + 1));
                let to = self.base_path.with_extension(format!("{}", i));
                if from.exists() {
                    fs::rename(from, to)?;
                } else {
                    last = i;
                }
            }

            // Move current file to .0
            if self.base_path.exists() {
                let backup = self.base_path.with_extension(format!("{}", last));
                fs::rename(&self.base_path, backup)?;
            }

            // Create new current file
            File::create(&self.base_path)
        }
    }

    /// Memory-based rotator for testing
    pub struct MemoryRotator {
        buffers: Vec<Vec<u8>>,
    }

    impl MemoryRotator {
        pub fn new() -> Self {
            Self {
                buffers: Vec::new(),
            }
        }

        pub fn get_buffer(&self, index: usize) -> Option<&[u8]> {
            self.buffers.get(index).map(|v| v.as_slice())
        }

        pub fn all_buffers(&self) -> &[Vec<u8>] {
            &self.buffers
        }
    }

    impl Rotator for MemoryRotator {
        type Writer = MemoryWriter;

        fn initial(&mut self) -> io::Result<Self::Writer> {
            self.buffers.push(Vec::new());
            Ok(MemoryWriter::new(self.buffers.len() - 1))
        }

        fn rotate(&mut self, current: Self::Writer) -> io::Result<Self::Writer> {
            // Finalize current buffer
            if let Some(buffer) = self.buffers.get_mut(current.index) {
                *buffer = current.buffer;
            }

            // Create new buffer
            self.buffers.push(Vec::new());
            Ok(MemoryWriter::new(self.buffers.len() - 1))
        }
    }

    pub struct MemoryWriter {
        buffer: Vec<u8>,
        index: usize,
    }

    impl MemoryWriter {
        fn new(index: usize) -> Self {
            Self {
                buffer: Vec::new(),
                index,
            }
        }
    }

    impl Write for MemoryWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.buffer.extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }
}

pub mod triggers {
    use super::*;

    pub struct Bytes {
        pub count: usize,
        pub limit: usize,
    }

    impl Bytes {
        pub fn new() -> Self {
            let byte_count_1_mib = 1048576;
            Self {
                count: 0,
                limit: byte_count_1_mib,
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
                let consumed = self.limit - self.count;
                Action::Rotate { consumed }
            } else {
                self.count += bytes.len();
                Action::None
            }
        }

        fn reset(&mut self) {
            self.count = 0;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn basic_file_rotation() {
        let rotator = rotators::NumberedSuffix::new("foo").max(3);
        let trigger = triggers::Bytes::new().limit(10);
        let mut fr = FileRotate::new(rotator, trigger).unwrap();

        write!(fr, "abcdefghijklmnopqrstuvwxyz0123456789").unwrap();

        assert_eq!("abcdefghij", fs::read_to_string("foo.0").unwrap());
        assert_eq!("klmnopqrst", fs::read_to_string("foo.1").unwrap());
        assert_eq!("uvwxyz0123", fs::read_to_string("foo.2").unwrap());
        assert_eq!("456789", fs::read_to_string("foo").unwrap());

        write!(fr, "!@#$%^&*[]_+").unwrap();

        assert_eq!("klmnopqrst", fs::read_to_string("foo.0").unwrap());
        assert_eq!("uvwxyz0123", fs::read_to_string("foo.1").unwrap());
        assert_eq!("456789!@#$", fs::read_to_string("foo.2").unwrap());
        assert_eq!("%^&*[]_+", fs::read_to_string("foo").unwrap());

        // Cleanup
        let _ = fs::remove_file("foo");
        let _ = fs::remove_file("foo.0");
        let _ = fs::remove_file("foo.1");
        let _ = fs::remove_file("foo.2");
        let _ = fs::remove_file("foo.3");

    }

    #[test]
    fn test_memory_rotator() {
        let rotator = rotators::MemoryRotator::new();
        let trigger = triggers::Bytes::new().limit(5);
        let mut fr = FileRotate::new(rotator, trigger).unwrap();

        write!(fr, "hello world test").unwrap();

        // Access the buffers
        assert_eq!(b"hello", fr.rotator.get_buffer(0).unwrap());
        assert_eq!(b" worl", fr.rotator.get_buffer(1).unwrap());
        assert_eq!(b"d tes", fr.rotator.get_buffer(2).unwrap());
    }

    #[test]
    fn test_line_count_trigger() {
        let mut trigger = LineCount::new().limit(3);

        // First two lines shouldn't trigger
        assert!(matches!(trigger.trigger(b"line 1\n"), Action::None));
        assert!(matches!(trigger.trigger(b"line 2\n"), Action::None));

        // Third line should trigger rotation
        match trigger.trigger(b"line 3\n") {
            Action::Rotate { consumed } => assert_eq!(consumed, 7),
            _ => panic!("Expected rotation"),
        }

        // After reset, should start counting again
        trigger.reset();
        assert!(matches!(trigger.trigger(b"new line 1\n"), Action::None));
    }

    #[test]
    fn test_trigger_combinator() {
        let size_trigger = triggers::Bytes::new().limit(10);
        let line_trigger = LineCount::new().limit(2);
        let mut combined = size_trigger.or(line_trigger);

        // First line shouldn't trigger either
        assert!(matches!(combined.trigger(b"short\n"), Action::None));

        // Second line should trigger line count limit
        match combined.trigger(b"line\n") {
            Action::Rotate { consumed } => assert_eq!(consumed, 4),
            _ => panic!("Expected rotation from line trigger"),
        }

        // After reset, test size trigger
        combined.reset();
        match combined.trigger(b"this is a very long line") {
            Action::Rotate { consumed } => assert_eq!(consumed, 10),
            _ => panic!("Expected rotation from size trigger"),
        }
    }

    #[test]
    fn test_combinator_with_memory() {
        let rotator = rotators::MemoryRotator::new();
        let byte_trigger = triggers::Bytes::new().limit(15);
        let line_trigger = LineCount::new().limit(2);
        let combined_trigger = byte_trigger.or(line_trigger);

        let mut fr = FileRotate::new(rotator, combined_trigger).unwrap();

        writeln!(fr, "First line").unwrap();    // 11 bytes, 1 line
        writeln!(fr, "Second line").unwrap();   // 12 bytes, 2 lines -> triggers line count
        write!(fr, "This is a longer third line").unwrap();
        writeln!(fr, "Fourth").unwrap();
        writeln!(fr, "Fifth").unwrap();         // Should trigger line count again
        write!(fr, "Final").unwrap();

        // Check the rotated buffers
        let buffer = fr.rotator.get_buffer(0).unwrap();
        let content = String::from_utf8_lossy(buffer);
        assert!(content.contains("First line"));
    }
}
