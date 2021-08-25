//! Write output to a file and rotate the files when limits have been exceeded.
//!
//! Defines a simple [std::io::Write] object that you can plug into your writers as middleware.
//!
//! # Content limit #
//!
//! Content limit specifies at what point a log file has to be rotated.
//!
//! ## Rotating by Lines ##
//!
//! We can rotate log files with the amount of lines as a limit, by using `ContentLimit::Lines`.
//!
//! ```
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix};
//! use std::{fs, io::Write};
//!
//! // Create a new log writer. The first argument is anything resembling a path. The
//! // basename is used for naming the log files.
//! //
//! // Here we choose to limit logs by 10 lines, and have at most 2 rotated log files. This
//! // makes the total amount of log files 3, since the original file is present as well.
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new(log_path.clone(), CountSuffix::new(2), ContentLimit::Lines(3));
//!
//! // Write a bunch of lines
//! writeln!(log, "Line 1: Hello World!");
//! for idx in 2..11 {
//!     writeln!(log, "Line {}", idx);
//! }
//!
//! assert_eq!("Line 10\n", fs::read_to_string(&log_path).unwrap());
//!
//! assert_eq!("Line 4\nLine 5\nLine 6\n", fs::read_to_string(&directory.join("my-log-file.2")).unwrap());
//! assert_eq!("Line 7\nLine 8\nLine 9\n", fs::read_to_string(&directory.join("my-log-file.1")).unwrap());
//! ```
//!
//! ## Rotating by Bytes ##
//!
//! Another method of rotation is by bytes instead of lines, with `ContentLimit::Bytes`.
//!
//! ```
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new("target/my-log-directory-bytes/my-log-file", CountSuffix::new(2), ContentLimit::Bytes(5));
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
//! Two rotation methods are provided, but any behaviour can be implemented with the `SuffixScheme`
//! trait.
//!
//! ## Basic count ##
//!
//! With `CountSuffix`, when the limit is reached in the main log file, the file is moved with
//! suffix `.1`, and subsequently numbered files are moved in a cascade.
//!
//! Here's an example with 1 byte limits:
//!
//! ```
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new(log_path.clone(), CountSuffix::new(3), ContentLimit::Bytes(1));
//!
//! write!(log, "A");
//! assert_eq!("A", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "B");
//! assert_eq!("A", fs::read_to_string(directory.join("my-log-file.1")).unwrap());
//! assert_eq!("B", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "C");
//! assert_eq!("A", fs::read_to_string(directory.join("my-log-file.2")).unwrap());
//! assert_eq!("B", fs::read_to_string(directory.join("my-log-file.1")).unwrap());
//! assert_eq!("C", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "D");
//! assert_eq!("A", fs::read_to_string(directory.join("my-log-file.3")).unwrap());
//! assert_eq!("B", fs::read_to_string(directory.join("my-log-file.2")).unwrap());
//! assert_eq!("C", fs::read_to_string(directory.join("my-log-file.1")).unwrap());
//! assert_eq!("D", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "E");
//! assert_eq!("B", fs::read_to_string(directory.join("my-log-file.3")).unwrap());
//! assert_eq!("C", fs::read_to_string(directory.join("my-log-file.2")).unwrap());
//! assert_eq!("D", fs::read_to_string(directory.join("my-log-file.1")).unwrap());
//! assert_eq!("E", fs::read_to_string(&log_path).unwrap());
//! ```
//!
//! ## Timestamp suffix ##
//!
//! With `TimestampSuffix`, when the limit is reached in the main log file, the file is moved with
//! suffix equal to the current timestamp (with the specified or a default format). If the
//! destination file name already exists, `.1` (and up) is appended.
//!
//! Note that this works somewhat different to `CountSuffix` because of lexical ordering concerns:
//! Higher numbers mean more recent logs, whereas `CountSuffix` works in the opposite way.
//! The reason for this is to keep the lexical ordering of log names consistent: Higher lexical value
//! means more recent.
//! This is of course all assuming that the format start with the year (or most significant
//! component).
//!
//! With this suffix scheme, you can also decide whether to delete old files based on the age of
//! their timestamp (`FileLimit::Age`), or just maximum number of files (`FileLimit::MaxFiles`).
//!
//! ```
//! use file_rotate::{FileRotate, ContentLimit, suffix::{TimestampSuffix, FileLimit}};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new(log_path.clone(), TimestampSuffix::default(FileLimit::MaxFiles(2)), ContentLimit::Bytes(1));
//!
//! write!(log, "A");
//! assert_eq!("A", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "B");
//! assert_eq!("A", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("B", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "C");
//! assert_eq!("A", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("B", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("C", fs::read_to_string(&log_path).unwrap());
//!
//! write!(log, "D");
//! assert_eq!("B", fs::read_to_string(&log.log_paths()[0]).unwrap());
//! assert_eq!("C", fs::read_to_string(&log.log_paths()[1]).unwrap());
//! assert_eq!("D", fs::read_to_string(&log_path).unwrap());
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

use std::{
    fs::{self, File},
    io::{self, Write},
    path::{Path, PathBuf},
};

/// Suffix scheme etc
pub mod suffix;

// ---

/// When to move files: Condition on which a file is rotated.
pub enum ContentLimit {
    /// Cut the log at the exact size in bytes.
    Bytes(usize),
    /// Cut the log file at line breaks.
    Lines(usize),
    /// Cut the log file after surpassing size in bytes (but having written a complete buffer from a write call.)
    BytesSurpassed(usize),
    // TODO: Custom(Fn(suffix: &str) -> bool)
    // Which can be used to test age in case of timestamps.
}

/// The main writer used for rotating logs.
pub struct FileRotate<S> {
    basepath: PathBuf,
    file: Option<File>,
    content_limit: ContentLimit,
    count: usize,
    suffix_scheme: S,
}

fn create_parent_dir(path: &Path) {
    if let Some(dirname) = path.parent() {
        if !dirname.exists() {
            fs::create_dir_all(dirname).expect("create dir");
        }
    }
}

impl<S: suffix::SuffixScheme> FileRotate<S> {
    /// Create a new [FileRotate].
    ///
    /// The basename of the `path` is used to create new log files by appending an extension of the
    /// form `.N`, where N is `0..=max_files`.
    ///
    /// `content_limit` specifies the limits for rotating a file.
    ///
    /// # Panics
    ///
    /// Panics if `bytes == 0` or `lines == 0`.
    pub fn new<P: AsRef<Path>>(path: P, suffix_scheme: S, content_limit: ContentLimit) -> Self {
        match content_limit {
            ContentLimit::Bytes(bytes) => {
                assert!(bytes > 0);
            }
            ContentLimit::Lines(lines) => {
                assert!(lines > 0);
            }
            ContentLimit::BytesSurpassed(bytes) => {
                assert!(bytes > 0);
            }
        };

        let basepath = path.as_ref().to_path_buf();
        create_parent_dir(&basepath);

        Self {
            file: File::create(&basepath).ok(),
            basepath,
            content_limit,
            count: 0,
            suffix_scheme,
        }
    }
    /// Get paths of rotated log files (excluding the original/current log file)
    pub fn log_paths(&mut self) -> Vec<PathBuf> {
        self.suffix_scheme.log_paths(&self.basepath)
    }

    fn rotate(&mut self) -> io::Result<()> {
        let suffix = self.suffix_scheme.rotate(&self.basepath);
        let path = PathBuf::from(format!("{}.{}", self.basepath.display(), suffix));

        create_parent_dir(&path);

        let _ = self.file.take();

        // TODO should handle this error (and others)
        let _ = fs::rename(&self.basepath, &path);

        self.file = Some(File::create(&self.basepath)?);
        self.count = 0;

        Ok(())
    }
}

impl<S: suffix::SuffixScheme> Write for FileRotate<S> {
    fn write(&mut self, mut buf: &[u8]) -> io::Result<usize> {
        let written = buf.len();
        match self.content_limit {
            ContentLimit::Bytes(bytes) => {
                while self.count + buf.len() > bytes {
                    let bytes_left = bytes - self.count;
                    if let Some(ref mut file) = self.file {
                        file.write_all(&buf[..bytes_left])?;
                    }
                    self.rotate()?;
                    buf = &buf[bytes_left..];
                }
                self.count += buf.len();
                if let Some(ref mut file) = self.file {
                    file.write_all(&buf)?;
                }
            }
            ContentLimit::Lines(lines) => {
                while let Some((idx, _)) = buf.iter().enumerate().find(|(_, byte)| *byte == &b'\n')
                {
                    if let Some(ref mut file) = self.file {
                        file.write_all(&buf[..idx + 1])?;
                    }
                    self.count += 1;
                    buf = &buf[idx + 1..];
                    if self.count >= lines {
                        self.rotate()?;
                    }
                }
                if let Some(ref mut file) = self.file {
                    file.write_all(buf)?;
                }
            }
            ContentLimit::BytesSurpassed(bytes) => {
                if self.count > bytes {
                    self.rotate()?
                }
                if let Some(ref mut file) = self.file {
                    file.write_all(&buf)?;
                }
                self.count += buf.len();
            }
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.file
            .as_mut()
            .map(|file| file.flush())
            .unwrap_or(Ok(()))
    }
}

#[cfg(test)]
mod tests {
    use super::{suffix::*, *};
    use tempdir::TempDir;

    // Just useful to debug why test doesn't succeed
    #[allow(dead_code)]
    fn list(dir: &Path) {
        let filenames = fs::read_dir(dir)
            .unwrap()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().is_file())
            .map(|entry| entry.file_name())
            .collect::<Vec<_>>();
        println!("Files on disk: {:?}", filenames);
    }

    #[test]
    fn timestamp_max_files_rotation() {
        let tmp_dir = TempDir::new("file-rotate-test").unwrap();
        let log_path = tmp_dir.path().join("log");
        let _ = fs::remove_dir_all(log_path.parent().unwrap());
        let mut log = FileRotate::new(
            &*log_path.to_string_lossy(),
            TimestampSuffix::default(FileLimit::MaxFiles(4)),
            ContentLimit::Lines(2),
        );

        // Write 9 lines
        // This should result in 5 files in total (4 rotated files). The main file will have one line.
        write!(log, "a\nb\nc\nd\ne\nf\ng\nh\ni\n").unwrap();
        let log_paths = log.log_paths();
        assert_eq!(log_paths.len(), 4);

        // Log names should be sorted. Low (old timestamp) to high (more recent timestamp)
        let mut log_paths_sorted = log_paths.clone();
        log_paths_sorted.sort();
        assert_eq!(log_paths, log_paths_sorted);

        assert_eq!("a\nb\n", fs::read_to_string(&log_paths[0]).unwrap());
        assert_eq!("c\nd\n", fs::read_to_string(&log_paths[1]).unwrap());
        assert_eq!("e\nf\n", fs::read_to_string(&log_paths[2]).unwrap());
        assert_eq!("g\nh\n", fs::read_to_string(&log_paths[3]).unwrap());
        assert_eq!("i\n", fs::read_to_string(&log_path).unwrap());

        // Write 4 more lines
        write!(log, "j\nk\nl\nm\n").unwrap();
        let log_paths = log.log_paths();
        assert_eq!(log_paths.len(), 4);
        let mut log_paths_sorted = log_paths.clone();
        log_paths_sorted.sort();
        assert_eq!(log_paths, log_paths_sorted);

        assert_eq!("e\nf\n", fs::read_to_string(&log_paths[0]).unwrap());
        assert_eq!("g\nh\n", fs::read_to_string(&log_paths[1]).unwrap());
        assert_eq!("i\nj\n", fs::read_to_string(&log_paths[2]).unwrap());
        assert_eq!("k\nl\n", fs::read_to_string(&log_paths[3]).unwrap());
        assert_eq!("m\n", fs::read_to_string(&log_path).unwrap());
    }
    #[test]
    fn count_max_files_rotation() {
        let tmp_dir = TempDir::new("file-rotate-test").unwrap();
        let parent = tmp_dir.path();
        let log_path = parent.join("log");
        let _ = fs::remove_dir_all(log_path.parent().unwrap());
        let mut log = FileRotate::new(
            &*log_path.to_string_lossy(),
            CountSuffix::new(4),
            ContentLimit::Lines(2),
        );

        // Write 9 lines
        // This should result in 5 files in total (4 rotated files). The main file will have one line.
        write!(log, "a\nb\nc\nd\ne\nf\ng\nh\ni\n").unwrap(); // 9 lines
        let log_paths = vec![
            parent.join("log.4"),
            parent.join("log.3"),
            parent.join("log.2"),
            parent.join("log.1"),
        ];
        assert_eq!(log_paths, log.log_paths());
        assert_eq!("a\nb\n", fs::read_to_string(&log_paths[0]).unwrap());
        assert_eq!("c\nd\n", fs::read_to_string(&log_paths[1]).unwrap());
        assert_eq!("e\nf\n", fs::read_to_string(&log_paths[2]).unwrap());
        assert_eq!("g\nh\n", fs::read_to_string(&log_paths[3]).unwrap());
        assert_eq!("i\n", fs::read_to_string(&log_path).unwrap());

        // Write 4 more lines
        write!(log, "j\nk\nl\nm\n").unwrap();
        list(parent);
        assert_eq!(log_paths, log.log_paths());

        assert_eq!("e\nf\n", fs::read_to_string(&log_paths[0]).unwrap());
        assert_eq!("g\nh\n", fs::read_to_string(&log_paths[1]).unwrap());
        assert_eq!("i\nj\n", fs::read_to_string(&log_paths[2]).unwrap());
        assert_eq!("k\nl\n", fs::read_to_string(&log_paths[3]).unwrap());
        assert_eq!("m\n", fs::read_to_string(&log_path).unwrap());
    }

    #[test]
    fn rotate_to_deleted_directory() {
        // NOTE: Only supported with count, not with timestamp suffix.
        let tmp_dir = TempDir::new("file-rotate-test").unwrap();
        let parent = tmp_dir.path();
        let log_path = parent.join("log");
        let _ = fs::remove_dir_all(log_path.parent().unwrap());
        let mut log = FileRotate::new(
            &*log_path.to_string_lossy(),
            CountSuffix::new(4),
            ContentLimit::Lines(1),
        );

        write!(log, "a\nb\n").unwrap();
        assert_eq!("", fs::read_to_string(&log_path).unwrap());
        assert_eq!("a\n", fs::read_to_string(&log.log_paths()[0]).unwrap());

        let _ = fs::remove_dir_all(parent);

        assert!(writeln!(log, "c").is_ok());

        log.flush().unwrap();

        writeln!(log, "d").unwrap();
        assert_eq!("", fs::read_to_string(&log_path).unwrap());
        assert_eq!("d\n", fs::read_to_string(&log.log_paths()[0]).unwrap());
    }

    #[test]
    fn write_complete_record_until_bytes_surpassed() {
        let _ = fs::remove_dir_all("target/surpassed_bytes");
        fs::create_dir("target/surpassed_bytes").unwrap();

        let mut log = FileRotate::new(
            "target/surpassed_bytes/log",
            TimestampSuffix::default(FileLimit::MaxFiles(100)),
            ContentLimit::BytesSurpassed(1),
        );

        write!(log, "0123456789").unwrap();
        log.flush().unwrap();
        assert!(Path::new("target/surpassed_bytes/log").exists());
        // shouldn't exist yet - because entire record was written in one shot
        assert!(log.log_paths().is_empty());

        // This should create the second file
        write!(log, "0123456789").unwrap();
        log.flush().unwrap();
        assert!(&log.log_paths()[0].exists());

        fs::remove_dir_all("target/surpassed_bytes").unwrap();
    }

    #[quickcheck_macros::quickcheck]
    fn arbitrary_lines(count: usize) {
        let _ = fs::remove_dir_all("target/arbitrary_lines");

        let count = count.max(1);
        let mut log = FileRotate::new(
            "target/arbitrary_lines/log",
            TimestampSuffix::default(FileLimit::MaxFiles(100)),
            ContentLimit::Lines(count),
        );

        for _ in 0..count - 1 {
            writeln!(log).unwrap();
        }

        log.flush().unwrap();
        assert!(log.log_paths().is_empty());
        writeln!(log).unwrap();
        assert!(Path::new(&log.log_paths()[0]).exists());

        fs::remove_dir_all("target/arbitrary_lines").unwrap();
    }

    #[quickcheck_macros::quickcheck]
    fn arbitrary_bytes(count: usize) {
        let _ = fs::remove_dir_all("target/arbitrary_bytes");
        fs::create_dir("target/arbitrary_bytes").unwrap();

        let count = count.max(1);
        let mut log = FileRotate::new(
            "target/arbitrary_bytes/log",
            TimestampSuffix::default(FileLimit::MaxFiles(100)),
            ContentLimit::Bytes(count),
        );

        for _ in 0..count {
            write!(log, "0").unwrap();
        }

        log.flush().unwrap();
        assert!(log.log_paths().is_empty());
        write!(log, "1").unwrap();
        assert!(Path::new(&log.log_paths()[0]).exists());

        fs::remove_dir_all("target/arbitrary_bytes").unwrap();
    }
}
