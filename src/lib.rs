//! Write output to a file and rotate the files when limits have been exceeded.
//!
//! Defines a simple [std::io::Write] object that you can plug into your writers as middleware.
//!
//! # Rotating by Lines #
//!
//! We can rotate log files by using the amount of lines as a limit.
//!
//! ```
//! use file_rotate::{FileRotate, RotationMode};
//! use std::{fs, io::Write};
//!
//! // Create a new log writer. The first argument is anything resembling a path. The
//! // basename is used for naming the log files.
//! //
//! // Here we choose to limit logs by 10 lines, and have at most 2 rotated log files. This
//! // makes the total amount of log files 4, since the original file is present as well as
//! // file 0.
//! let mut log = FileRotate::new("target/my-log-directory-lines/my-log-file", RotationMode::Lines(3), 2);
//!
//! // Write a bunch of lines
//! writeln!(log, "Line 1: Hello World!");
//! for idx in 2..11 {
//!     writeln!(log, "Line {}", idx);
//! }
//!
//! assert_eq!("Line 10\n", fs::read_to_string("target/my-log-directory-lines/my-log-file").unwrap());
//!
//! assert_eq!("Line 4\nLine 5\nLine 6\n", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("Line 7\nLine 8\nLine 9\n", fs::read_to_string(&log.log_paths()[1]).unwrap());
//!
//! fs::remove_dir_all("target/my-log-directory-lines");
//! ```
//!
//! # Rotating by Bytes #
//!
//! Another method of rotation is by bytes instead of lines.
//!
//! ```
//! use file_rotate::{FileRotate, RotationMode};
//! use std::{fs, io::Write};
//!
//! fs::create_dir("target/my-log-directory-bytes");
//!
//! let mut log = FileRotate::new("target/my-log-directory-bytes/my-log-file", RotationMode::Bytes(5), 2);
//!
//! writeln!(log, "Test file");
//!
//! assert_eq!("Test ", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("file\n", fs::read_to_string("target/my-log-directory-bytes/my-log-file").unwrap());
//!
//! fs::remove_dir_all("target/my-log-directory-bytes");
//! ```
//!
//! # Rotation Method #
//!
//! The rotation method used is to always write to the base path, and then move the file to a new
//! location when the limit is exceeded. The moving occurs in the sequence 0, 1, 2, n, 0, 1, 2...
//!
//! Here's an example with 1 byte limits:
//!
//! ```
//! use file_rotate::{FileRotate, RotationMode};
//! use std::{fs, io::Write};
//!
//! fs::create_dir("target/my-log-directory-small");
//!
//! let mut log = FileRotate::new("target/my-log-directory-small/my-log-file", RotationMode::Bytes(1), 3);
//!
//! write!(log, "A");
//! assert_eq!("A", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//! write!(log, "B");
//! assert_eq!("A", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("B", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//! write!(log, "C");
//! assert_eq!("A", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("B", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("C", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//! write!(log, "D");
//! assert_eq!("A", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("B", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("C", fs::read_to_string(&log.log_paths()[2]).unwrap());
//! assert_eq!("D", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//! write!(log, "E");
//! assert_eq!("B", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("C", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("D", fs::read_to_string(&log.log_paths()[2]).unwrap());
//! assert_eq!("E", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//!
//! // Here we overwrite the `1` file since we're out of log files, restarting the sequencing.
//! // We keep file 0 since this is the initial file. It may contain system startup information we
//! // do not want to lose.
//! write!(log, "F");
//! assert_eq!("C", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("D", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("E", fs::read_to_string(&log.log_paths()[2]).unwrap());
//! assert_eq!("F", fs::read_to_string("target/my-log-directory-small/my-log-file").unwrap());
//!
//! fs::remove_dir_all("target/my-log-directory-small");
//! ```
//!
//! # Filesystem Errors #
//!
//! If the directory containing the logs is deleted or somehow made inaccessible then the rotator
//! will simply continue operating without fault. When a rotation occurs, it attempts to open a
//! file in the directory. If it can, it will just continue logging. If it can't then the written
//! date is sent to the void.
//!
//! This logger never panics.
#![deny(
    missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    unused_import_braces,
    unused_qualifications
)]

use chrono::Local;
use std::{
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
};

// ---

/// Condition on which a file is rotated.
pub enum RotationMode {
    /// Cut the log at the exact size in bytes.
    Bytes(usize),
    /// Cut the log file at line breaks.
    Lines(usize),
    /// Cut the log file after surpassing size in bytes (but having written a complete buffer from a write call.)
    BytesSurpassed(usize),
}

/// The main writer used for rotating logs.
pub struct FileRotate {
    basename: PathBuf,
    log_paths: Vec<PathBuf>,
    file: Option<File>,
    max_file_number: usize,
    mode: RotationMode,
    count: usize,
}

fn create_dir(path: &PathBuf) {
    if let Some(dirname) = path.parent() {
        if !dirname.exists() {
            fs::create_dir_all(dirname).expect("create dir");
        }
    }
}

impl FileRotate {
    /// Create a new [FileRotate].
    ///
    /// The basename of the `path` is used to create new log files by appending an extension of the
    /// form `.N`, where N is `0..=max_file_number`.
    ///
    /// `rotation_mode` specifies the limits for rotating a file.
    ///
    /// # Panics
    ///
    /// Panics if `bytes == 0` or `lines == 0`.
    pub fn new<P: AsRef<Path>>(path: P, rotation_mode: RotationMode, max_files: usize) -> Self {
        match rotation_mode {
            RotationMode::Bytes(bytes) => {
                assert!(bytes > 0);
            }
            RotationMode::Lines(lines) => {
                assert!(lines > 0);
            }
            RotationMode::BytesSurpassed(bytes) => {
                assert!(bytes > 0);
            }
        };

        let basename = path.as_ref().to_path_buf();
        create_dir(&basename);

        let mut rotater = Self {
            log_paths: Vec::with_capacity(max_files),
            file: None,
            basename,
            max_file_number: max_files,
            mode: rotation_mode,
            count: 0,
        };

        let _ = rotater.rotate(true);

        rotater
    }

    fn new_ts_path(&self, num: Option<usize>) -> PathBuf {
        match num {
            None => PathBuf::from(format!(
                "{}-{}",
                self.basename.to_string_lossy(),
                Local::now().format("%Y%m%dT%H%M%S")
            )),
            Some(n) => PathBuf::from(format!(
                "{}-{}-{}",
                self.basename.to_string_lossy(),
                Local::now().format("%Y%m%dT%H%M%S"),
                n
            )),
        }
    }

    fn rotate(&mut self, initial: bool) -> io::Result<()> {
        let mut path = self.new_ts_path(None);

        let mut counter = 1;
        while self.log_paths.contains(&path) {
            path = self.new_ts_path(Some(counter));
            counter += 1;
        }

        create_dir(&path);

        let _ = self.file.take();

        let _ = fs::rename(&self.basename, &path);
        self.file = Some(File::create(&self.basename)?);

        if !initial {
            self.log_paths.push(path);

            while self.log_paths.len() > self.max_file_number {
                let path_to_remove = self.log_paths.remove(0);
                let _ = fs::remove_file(path_to_remove);
            }
        }
        self.count = 0;

        Ok(())
    }

    /// Return all log paths. elder first, newer last.
    pub fn log_paths(&self) -> &[PathBuf] {
        &self.log_paths
    }
}

impl Write for FileRotate {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let written = buf.len();
        match self.mode {
            RotationMode::Bytes(bytes) => {
                while self.count + buf.len() > bytes {
                    let bytes_left = bytes - self.count;
                    if let Some(Err(err)) = self
                        .file
                        .as_mut()
                        .map(|file| file.write(&buf[..bytes_left]))
                    {
                        return Err(err);
                    }
                    self.rotate(false)?;
                    buf = &buf[bytes_left..];
                }
                self.count += buf.len();
                if let Some(Err(err)) = self.file.as_mut().map(|file| file.write(&buf[..])) {
                    return Err(err);
                }
            }
            RotationMode::Lines(lines) => {
                while let Some((idx, _)) = buf.iter().enumerate().find(|(_, byte)| *byte == &b'\n')
                {
                    if let Some(Err(err)) =
                        self.file.as_mut().map(|file| file.write(&buf[..idx + 1]))
                    {
                        return Err(err);
                    }
                    self.count += 1;
                    buf = &buf[idx + 1..];
                    if self.count >= lines {
                        self.rotate(false)?;
                    }
                }
                if let Some(Err(err)) = self.file.as_mut().map(|file| file.write(buf)) {
                    return Err(err);
                }
            }
            RotationMode::BytesSurpassed(bytes) => {
                if self.count > bytes {
                    self.rotate(false)?
                }
                if let Some(Err(err)) = self.file.as_mut().map(|file| file.write(&buf)) {
                    return Err(err);
                }
                self.count += buf.len();
            }
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        match self.file.as_mut().map(|file| file.flush()) {
            Some(Err(err)) => Err(err),
            _ => Ok(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timestamp_suffix() {
        let _ = fs::remove_dir_all("target/rotate1");
        let mut rot = FileRotate::new("target/rotate1/ts-suffix.log", RotationMode::Lines(2), 10);
        write!(rot, "a\nb\nc\nd\n").unwrap();
        assert_eq!("a\nb\n", fs::read_to_string(&rot.log_paths()[0]).unwrap());
        assert_eq!("c\nd\n", fs::read_to_string(&rot.log_paths()[1]).unwrap());
    }

    #[test]
    fn rotate_to_deleted_directory() {
        let _ = fs::remove_dir_all("target/rotate2");
        fs::create_dir("target/rotate2").expect("create dir");

        let mut rot = FileRotate::new("target/rotate2/log", RotationMode::Lines(1), 1);
        writeln!(rot, "a").expect("write a");
        assert_eq!("", fs::read_to_string("target/rotate2/log").unwrap());
        assert_eq!("a\n", fs::read_to_string(&rot.log_paths()[0]).unwrap());

        let _ = fs::remove_dir_all("target/rotate2");

        assert!(writeln!(rot, "b").is_ok());

        rot.flush().unwrap();

        writeln!(rot, "c").unwrap();
        assert_eq!("", fs::read_to_string("target/rotate2/log").unwrap());

        writeln!(rot, "d").unwrap();
        assert_eq!("", fs::read_to_string("target/rotate2/log").unwrap());
        assert_eq!("d\n", fs::read_to_string(&rot.log_paths()[0]).unwrap());
    }

    #[test]
    fn write_complete_record_until_bytes_surpassed() {
        let _ = fs::remove_dir_all("target/surpassed_bytes");
        fs::create_dir("target/surpassed_bytes").unwrap();

        let mut rot = FileRotate::new(
            "target/surpassed_bytes/log",
            RotationMode::BytesSurpassed(1),
            1,
        );

        write!(rot, "0123456789").unwrap();
        rot.flush().unwrap();
        assert!(Path::new("target/surpassed_bytes/log").exists());
        // shouldn't exist yet - because entire record was written in one shot
        assert!(rot.log_paths().is_empty());

        // This should create the second file
        write!(rot, "0123456789").unwrap();
        rot.flush().unwrap();
        assert!(&rot.log_paths()[0].exists());

        fs::remove_dir_all("target/surpassed_bytes").unwrap();
    }

    #[quickcheck_macros::quickcheck]
    fn arbitrary_lines(count: usize) {
        let _ = fs::remove_dir_all("target/arbitrary_lines");

        let count = count.max(1);
        let mut rot = FileRotate::new("target/arbitrary_lines/log", RotationMode::Lines(count), 1);

        for _ in 0..count - 1 {
            writeln!(rot).unwrap();
        }

        rot.flush().unwrap();
        assert!(rot.log_paths().is_empty());
        writeln!(rot).unwrap();
        assert!(Path::new(&rot.log_paths()[0]).exists());

        fs::remove_dir_all("target/arbitrary_lines").unwrap();
    }

    #[quickcheck_macros::quickcheck]
    fn arbitrary_bytes(count: usize) {
        let _ = fs::remove_dir_all("target/arbitrary_bytes");
        fs::create_dir("target/arbitrary_bytes").unwrap();

        let count = count.max(1);
        let mut rot = FileRotate::new("target/arbitrary_bytes/log", RotationMode::Bytes(count), 1);

        for _ in 0..count {
            write!(rot, "0").unwrap();
        }

        rot.flush().unwrap();
        assert!(rot.log_paths().is_empty());
        write!(rot, "1").unwrap();
        assert!(Path::new(&rot.log_paths()[0]).exists());

        fs::remove_dir_all("target/arbitrary_bytes").unwrap();
    }
}
