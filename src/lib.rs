//! File rotation for writers with modular triggers and rotators.
//!
//! This crate provides a simple `std::io::Write` implementation that automatically
//! rotates outputs based on pluggable conditions (triggers) and rotation schemes
//! (rotators). It is designed to be modular, testable, and easy to extend.
//!
//! # Overview
//!
//! - Triggers decide when to rotate (e.g., by bytes, by lines, by delimiter, by time interval).
//! - Rotators decide how to rotate (e.g., numbered suffix files, in-memory for tests).
//!
//! You compose them with `FileRotate<Rotator, Trigger>` which implements `Write`.
//!
//! # Design principles
//!
//! - Separation of concerns: triggers decide when to rotate; rotators decide how.
//! - Deterministic testing: time sources are abstracted behind a `Clock` trait and
//!   file-based tests use temporary directories.
//! - Backpressure-free writes: rotation happens inline during `write`, splitting the
//!   input buffer into consumed and remaining parts.
//! - Minimal allocations: triggers inspect slices; rotators are responsible for IO.
//!
//! # Examples
//!
//! ## Rotate by bytes with numbered suffix files
//!
//! Rotates every 10 bytes, keeping up to 3 rotated files alongside the base file.
//!
//! ```
//! use std::io::Write;
//! use file_rotate::{FileRotate, rotators::NumberedSuffix, triggers::Bytes};
//!
//! let dir = tempfile::tempdir().unwrap();
//! let base = dir.path().join("app.log");
//!
//! let rotator = NumberedSuffix::new(base.clone()).max(3);
//! let trigger = Bytes::new().limit(10);
//! let mut log = FileRotate::new(rotator, trigger).unwrap();
//!
//! write!(log, "abcdefghijklmnopqrstuvwxyz0123456789").unwrap();
//!
//! assert_eq!("abcdefghij", std::fs::read_to_string(base.with_extension("0")).unwrap());
//! assert_eq!("klmnopqrst", std::fs::read_to_string(base.with_extension("1")).unwrap());
//! assert_eq!("uvwxyz0123", std::fs::read_to_string(base.with_extension("2")).unwrap());
//! assert_eq!("456789",     std::fs::read_to_string(&base).unwrap());
//! ```
//!
//! ## Rotate on a delimiter
//!
//! Rotates whenever a given byte sequence is seen. Matches can span multiple writes.
//!
//! ```
//! use std::io::Write;
//! use file_rotate::{FileRotate, rotators::NumberedSuffix, triggers::Delimiter};
//!
//! let dir = tempfile::tempdir().unwrap();
//! let base = dir.path().join("events");
//! let rotator = NumberedSuffix::new(base.clone()).max(2);
//! let mut log = FileRotate::new(rotator, Delimiter::new("END\n", true)).unwrap();
//!
//! write!(log, "part1 END\npart2 END\n").unwrap();
//! // Two rotations occurred: newest data is in .1 and base is empty
//! assert_eq!("part1 END\n", std::fs::read_to_string(base.with_extension("0")).unwrap());
//! assert_eq!("part2 END\n", std::fs::read_to_string(base.with_extension("1")).unwrap());
//! assert_eq!("", std::fs::read_to_string(&base).unwrap());
//! ```
//!
//! ## Rotate at fixed intervals (mockable clock)
//!
//! Uses a clock trait so time can be mocked in tests.
//!
//! ```
//! use std::cell::Cell;
//! use std::io::Write;
//! use std::time::Duration;
//! use file_rotate::{FileRotate, rotators::MemoryRotator, triggers::{Interval, Clock}};
//!
//! #[derive(Clone)]
//! struct StepClock(Cell<Duration>);
//! impl Clock for StepClock { fn now(&self) -> Duration { self.0.get() } }
//!
//! let clock = StepClock(Cell::new(Duration::from_secs(0)));
//! let trigger = Interval::new(clock.clone(), Duration::from_secs(1));
//! let rotator = MemoryRotator::new();
//! let mut log = FileRotate::new(rotator, trigger).unwrap();
//!
//! // First write sets the initial time reference.
//! write!(log, "hello").unwrap();
//! // Advance time to exceed the interval, causing rotation on the next write.
//! clock.0.set(Duration::from_secs(2));
//! write!(log, "world").unwrap();
//! ```
//!
//! # More examples
//!
//! You can run additional examples from the examples/ directory:
//!
//! - `cargo run --example rotate_bytes`
//! - `cargo run --example rotate_delimiter`
//! - `cargo run --example rotate_interval`
//!
//! # Testing and determinism
//!
//! - Use `tempfile::tempdir()` to isolate file-based tests and prevent cross-test
//!   interference.
//! - For time-based triggers, implement `Clock` and inject a mock clock into `Interval`.
//! - When asserting file contents, flush the writer first (e.g., `writer.flush()?`).
//!
//! # Error handling
//!
//! - `FileRotate::new` returns IO errors from the underlying rotator during the initial
//!   writer creation.
//! - Errors during rotation propagate from the rotator's `rotate` method.
//! - Attempting to write or flush without an active writer returns an error.
use std::io::{self, Write};

#[cfg(test)]
pub mod test_support;

/// Handles *when* to rotate.
pub trait Trigger {
    /// Additional metadata that can be exposed by a trigger for use by a modifier.
    type Meta;

    /// Called for every write. Counts properties of the output stream and decides whether to perform a rotation or not.
    fn trigger(&mut self, bytes: &[u8]) -> Action;
    /// Reset the current state. Called when `trigger` returns Action::Rotate.
    fn reset(&mut self);
    /// Observe the provided bytes and return metadata for the modifier.
    fn observe(&mut self, _bytes: &[u8]) -> Self::Meta;
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
    // Optional modifier: when present, each written line is prefixed (or otherwise
    // modified) using bytes produced by this closure based on trigger metadata.
    modifier: Option<Box<dyn FnMut(&T::Meta) -> Vec<u8>>>,
    // Tracks whether the next byte to be written is at the start of a line.
    at_line_start: bool,
}

impl<R, T> FileRotate<R, T>
where
    R: Rotator,
    T: Trigger,
{
    pub fn new(mut rotator: R, trigger: T) -> io::Result<Self> {
        let writer = Some(rotator.initial()?);
        Ok(Self {
            writer,
            rotator,
            trigger,
            modifier: None,
            at_line_start: true,
        })
    }

    /// Construct with a line modifier that runs at the start of each line.
    /// The closure is invoked at the moment of writing with the trigger's
    /// current metadata, so it can produce a prefix or other transformation.
    pub fn with_modifier(
        mut rotator: R,
        trigger: T,
        modifier: Box<dyn FnMut(&T::Meta) -> Vec<u8>>,
    ) -> io::Result<Self> {
        let writer = Some(rotator.initial()?);
        Ok(Self {
            writer,
            rotator,
            trigger,
            modifier: Some(modifier),
            at_line_start: true,
        })
    }

    fn write_with_modifier_to(
        writer: &mut R::Writer,
        data: &[u8],
        modifier: &mut Option<Box<dyn FnMut(&T::Meta) -> Vec<u8>>>,
        at_line_start: &mut bool,
        meta: &T::Meta,
    ) -> io::Result<()> {
        if data.is_empty() {
            return Ok(());
        }
        match modifier.as_mut() {
            None => writer.write_all(data),
            Some(modifier) => {
                let mut i = 0;
                // If we are at the start of a line, write the prefix first.
                if *at_line_start {
                    let p = (modifier)(meta);
                    writer.write_all(&p)?;
                    *at_line_start = false;
                }
                while i < data.len() {
                    if let Some(rel) = data[i..].iter().position(|&b| b == b'\n') {
                        let end = i + rel + 1; // include the newline
                        writer.write_all(&data[i..end])?;
                        i = end;
                        if i < data.len() {
                            // New line starts immediately; emit prefix for next line.
                            let p = (modifier)(meta);
                            writer.write_all(&p)?;
                            *at_line_start = false;
                        } else {
                            // Buffer ended exactly at a newline; next write is at line start.
                            *at_line_start = true;
                        }
                    } else {
                        writer.write_all(&data[i..])?;
                        break;
                    }
                }
                Ok(())
            }
        }
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
            // Allow the trigger (and its children) to update metadata for this chunk.
            let meta = self.trigger.observe(&buf[begin..]);
            match self.trigger.trigger(&buf[begin..]) {
                Action::Rotate { consumed } => {
                    // Take writer, write consumed bytes (with optional prefix), then rotate.
                    let mut writer = self
                        .writer
                        .take()
                        .ok_or_else(|| io::Error::other("No writer available"))?;

                    // Perform the write into current writer
                    let write_res = Self::write_with_modifier_to(
                        &mut writer,
                        &buf[begin..begin + consumed],
                        &mut self.modifier,
                        &mut self.at_line_start,
                        &meta,
                    );

                    // Rotate regardless; if write failed, propagate after rotation to keep state consistent
                    let rotated = self.rotator.rotate(writer);
                    match (write_res, rotated) {
                        (Ok(()), Ok(new_writer)) => {
                            self.writer = Some(new_writer);
                        }
                        (Err(e), Ok(new_writer)) => {
                            self.writer = Some(new_writer);
                            return Err(e);
                        }
                        (Ok(()), Err(e)) => {
                            // reinstate no writer; return error
                            self.writer = None;
                            return Err(e);
                        }
                        (Err(e1), Err(_e2)) => {
                            self.writer = None;
                            return Err(e1);
                        }
                    }

                    begin += consumed;
                    self.trigger.reset();
                }
                Action::None => {
                    // Take writer to avoid borrowing conflicts, write remaining, then put back
                    let mut writer = self
                        .writer
                        .take()
                        .ok_or_else(|| io::Error::other("No writer available"))?;
                    let res = Self::write_with_modifier_to(
                        &mut writer,
                        &buf[begin..],
                        &mut self.modifier,
                        &mut self.at_line_start,
                        &meta,
                    );
                    self.writer = Some(writer);
                    res?;
                    return Ok(buf.len());
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        if let Some(writer) = &mut self.writer {
            writer.flush()
        } else {
            Err(io::Error::other("No writer to flush"))
        }
    }
}

// Modules split out for triggers and rotators
pub mod rotators;
pub mod triggers;

// Re-export commonly used items at crate root for convenience/back-compat
pub use rotators::{DatedSuffix, MemoryRotator, MemoryWriter, NumberedSuffix};
pub use triggers::{
    Bytes, Clock, Delimiter, Interval, LineCount, RealClock, TriggerCombinator, TriggerExt,
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn basic_file_rotation() {
        // Isolate files into a unique temp directory per test run
        let dir = tempdir().unwrap();
        let base = dir.path().join("foo");
        let rotator = rotators::NumberedSuffix::new(base.clone()).max(3);
        let trigger = triggers::Bytes::new().limit(10);
        let mut fr = FileRotate::new(rotator, trigger).unwrap();

        write!(fr, "abcdefghijklmnopqrstuvwxyz0123456789").unwrap();
        // Flush to ensure all IO is visible before reading
        fr.flush().unwrap();

        assert_eq!(
            "abcdefghij",
            fs::read_to_string(base.with_extension("0")).unwrap()
        );
        assert_eq!(
            "klmnopqrst",
            fs::read_to_string(base.with_extension("1")).unwrap()
        );
        assert_eq!(
            "uvwxyz0123",
            fs::read_to_string(base.with_extension("2")).unwrap()
        );
        assert_eq!("456789", fs::read_to_string(&base).unwrap());

        write!(fr, "!@#$%^&*[]_+").unwrap();

        assert_eq!(
            "klmnopqrst",
            fs::read_to_string(base.with_extension("0")).unwrap()
        );
        assert_eq!(
            "uvwxyz0123",
            fs::read_to_string(base.with_extension("1")).unwrap()
        );
        assert_eq!(
            "456789!@#$",
            fs::read_to_string(base.with_extension("2")).unwrap()
        );
        assert_eq!("%^&*[]_+", fs::read_to_string(&base).unwrap());
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

        writeln!(fr, "First line").unwrap(); // 11 bytes, 1 line
        writeln!(fr, "Second line").unwrap(); // 12 bytes, 2 lines -> triggers line count
        write!(fr, "This is a longer third line").unwrap();
        writeln!(fr, "Fourth").unwrap();
        writeln!(fr, "Fifth").unwrap(); // Should trigger line count again
        write!(fr, "Final").unwrap();

        // Check the rotated buffers
        let buffer = fr.rotator.get_buffer(0).unwrap();
        let content = String::from_utf8_lossy(buffer);
        assert!(content.contains("First line"));
    }

    #[test]
    fn test_modifier_with_interval_meta() {
        use crate::test_support::time::StepClock;
        use crate::triggers::Bytes as BytesTrigger;
        use crate::triggers::Interval;
        use std::cell::Cell;
        use std::time::Duration;

        let cell = std::rc::Rc::new(Cell::new(Duration::from_secs(0)));
        let clock = StepClock::new(cell.clone());
        let interval = Interval::new(clock.clone(), Duration::from_secs(60));
        let trigger = interval.or(BytesTrigger::new().limit(2));
        let rotator = rotators::MemoryRotator::new();

        let mut fr = FileRotate::with_modifier(
            rotator,
            trigger,
            Box::new(|meta: &(std::time::Duration, ())| {
                let (t, _) = meta;
                format!("t={} ", t.as_secs()).into_bytes()
            }),
        )
        .unwrap();

        // First write at t=0
        writeln!(fr, "A").unwrap();
        // Advance time and write again; combinator.observe ensures interval's shared time updates
        clock.0.set(Duration::from_secs(5));
        writeln!(fr, "B").unwrap();

        // Each write is 2 bytes including newline, so Bytes(2) fires each time.
        let buf0 = fr.rotator.get_buffer(0).unwrap();
        let buf1 = fr.rotator.get_buffer(1).unwrap();
        assert_eq!(buf0, b"t=0 A\n");
        assert_eq!(buf1, b"t=5 B\n");
    }
}
