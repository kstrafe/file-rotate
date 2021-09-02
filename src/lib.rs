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
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix, compression::Compression};
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
//! let mut log = FileRotate::new(log_path.clone(), CountSuffix::new(2), ContentLimit::Lines(3), Compression::None);
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
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix, compression::Compression};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new("target/my-log-directory-bytes/my-log-file", CountSuffix::new(2), ContentLimit::Bytes(5), Compression::None);
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
//! use file_rotate::{FileRotate, ContentLimit, suffix::CountSuffix, compression::Compression};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new(log_path.clone(), CountSuffix::new(3), ContentLimit::Bytes(1), Compression::None);
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
//! use file_rotate::{FileRotate, ContentLimit, suffix::{TimestampSuffixScheme, FileLimit},
//! compression::Compression};
//! use std::{fs, io::Write};
//!
//! # let directory = tempdir::TempDir::new("rotation-doc-test").unwrap();
//! # let directory = directory.path();
//! let log_path = directory.join("my-log-file");
//!
//! let mut log = FileRotate::new(log_path.clone(), TimestampSuffixScheme::default(FileLimit::MaxFiles(2)), ContentLimit::Bytes(1), Compression::None);
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
//! If you use timestamps as suffix, you can also configure files to be removed as they reach a
//! certain age. For example:
//! ```rust
//! use file_rotate::suffix::{TimestampSuffixScheme, FileLimit};
//! TimestampSuffixScheme::default(FileLimit::Age(chrono::Duration::weeks(1)));
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

use compression::*;
use std::{
    cmp::Ordering,
    collections::BTreeSet,
    fs::{self, File, OpenOptions},
    io::{self, Write},
    path::{Path, PathBuf},
};
use suffix::Representation;

/// Compression
pub mod compression;
/// Suffix scheme etc
pub mod suffix;
#[cfg(test)]
mod tests;

// ---

/// When to move files: Condition on which a file is rotated.
pub enum ContentLimit {
    /// Cut the log at the exact size in bytes.
    Bytes(usize),
    /// Cut the log file at line breaks.
    Lines(usize),
    /// Cut the log file after surpassing size in bytes (but having written a complete buffer from a write call.)
    BytesSurpassed(usize),
}

#[derive(Clone, Debug, Eq)]
struct SuffixInfo<Repr> {
    pub suffix: Repr,
    pub compressed: bool,
}
impl<R: PartialEq> PartialEq for SuffixInfo<R> {
    fn eq(&self, other: &Self) -> bool {
        self.suffix == other.suffix
    }
}

impl<Repr: Representation> SuffixInfo<Repr> {
    pub fn to_path(&self, basepath: &Path) -> PathBuf {
        let path = self.suffix.to_path(basepath);
        if self.compressed {
            PathBuf::from(format!("{}.gz", path.display()))
        } else {
            path
        }
    }
}

impl<Repr: Representation> Ord for SuffixInfo<Repr> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.suffix.cmp(&other.suffix)
    }
}
impl<Repr: Representation> PartialOrd for SuffixInfo<Repr> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// The main writer used for rotating logs.
pub struct FileRotate<S: suffix::SuffixScheme> {
    basepath: PathBuf,
    file: Option<File>,
    content_limit: ContentLimit,
    count: usize,
    compression: Compression,
    suffix_scheme: S,
    /// The bool is whether or not there's a .gz suffix to the filename
    suffixes: BTreeSet<SuffixInfo<S::Repr>>,
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
    pub fn new<P: AsRef<Path>>(
        path: P,
        suffix_scheme: S,
        content_limit: ContentLimit,
        compression: Compression,
    ) -> Self {
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
        fs::create_dir_all(basepath.parent().unwrap()).expect("create dir");

        let mut s = Self {
            file: None,
            basepath,
            content_limit,
            count: 0,
            compression,
            suffixes: BTreeSet::new(),
            suffix_scheme,
        };
        s.ensure_log_directory_exists();
        s.scan_suffixes();
        s
    }
    fn ensure_log_directory_exists(&mut self) {
        let path = self.basepath.parent().unwrap();
        if !path.exists() {
            let _ = fs::create_dir_all(path).expect("create dir");
            self.scan_suffixes();
        }
        if !self.basepath.exists() || self.file.is_none() {
            self.file = OpenOptions::new()
                .write(true)
                .create(true)
                .append(true)
                .open(&self.basepath)
                .ok();
        }
    }
    fn scan_suffixes(&mut self) {
        let mut suffixes = BTreeSet::new();
        let filename_prefix = &*self
            .basepath
            .file_name()
            .expect("basepath.file_name()")
            .to_string_lossy();
        let parent = self.basepath.parent().unwrap();
        let filenames = std::fs::read_dir(parent)
            .unwrap()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().is_file())
            .map(|entry| entry.file_name());
        for filename in filenames {
            let filename = filename.to_string_lossy();
            if !filename.starts_with(&filename_prefix) {
                continue;
            }
            let (filename, compressed) = Self::prepare_filename(&*filename);
            let suffix_str = filename.strip_prefix(&format!("{}.", filename_prefix));
            if let Some(suffix) = suffix_str.and_then(|s| self.suffix_scheme.parse(s)) {
                suffixes.insert(SuffixInfo { suffix, compressed });
            }
        }
        self.suffixes = suffixes;
    }
    fn prepare_filename(path: &str) -> (&str, bool) {
        path.strip_prefix(".gz")
            .map(|x| (x, true))
            .unwrap_or((path, false))
    }
    /// Get paths of rotated log files (excluding the original/current log file), ordered from
    /// oldest to most recent
    pub fn log_paths(&mut self) -> Vec<PathBuf> {
        self.suffixes
            .iter()
            .rev()
            .map(|suffix| suffix.to_path(&self.basepath))
            .collect::<Vec<_>>()
    }

    /// Recursive function that keeps moving files if there's any file name collision.
    /// If `suffix` is `None`, it moves from basepath to next suffix given by the SuffixScheme
    /// Assumption: Any collision in file name is due to an old log file.
    ///
    /// Returns the suffix of the new file (the last suffix after possible cascade of renames).
    fn move_file_with_suffix(
        &mut self,
        old_suffix_info: Option<SuffixInfo<S::Repr>>,
    ) -> io::Result<SuffixInfo<S::Repr>> {
        // NOTE: this newest_suffix is there only because TimestampSuffixScheme specifically needs
        // it. Otherwise it might not be necessary to provide this to `rotate_file`. We could also
        // have passed the internal BTreeMap itself, but it would require to make SuffixInfo `pub`.

        let newest_suffix = self.suffixes.iter().next().map(|info| &info.suffix);

        let new_suffix = self.suffix_scheme.rotate_file(
            &self.basepath,
            newest_suffix,
            &old_suffix_info.clone().map(|i| i.suffix),
        )?;

        // The destination file/path eventual .gz suffix must match the source path
        let new_suffix_info = SuffixInfo {
            suffix: new_suffix,
            compressed: old_suffix_info
                .as_ref()
                .map(|x| x.compressed)
                .unwrap_or(false),
        };
        let new_path = new_suffix_info.to_path(&self.basepath);

        // Whatever exists that would block a move to the new suffix
        let existing_suffix_info = self.suffixes.get(&new_suffix_info).cloned();

        // Move destination file out of the way if it exists
        let newly_created_suffix = if let Some(existing_suffix_info) = existing_suffix_info {
            // We might move files in a way that the destination path doesn't equal the path that
            // was replaced. Due to possible `.gz`, a "conflicting" file doesn't mean that paths
            // are equal.
            self.suffixes.replace(new_suffix_info);
            // Recurse to move conflicting file.
            self.move_file_with_suffix(Some(existing_suffix_info))?
        } else {
            new_suffix_info
        };

        let old_path = match old_suffix_info {
            Some(suffix) => suffix.to_path(&self.basepath),
            None => self.basepath.clone(),
        };

        // Do the move
        assert!(old_path.exists());
        assert!(!new_path.exists());
        fs::rename(old_path, new_path)?;

        Ok(newly_created_suffix)
    }

    fn rotate(&mut self) -> io::Result<()> {
        self.ensure_log_directory_exists();

        let _ = self.file.take();

        // This function will always create a new file. Returns suffix of that file
        let new_suffix_info = self.move_file_with_suffix(None)?;
        self.suffixes.insert(new_suffix_info);

        self.file = Some(File::create(&self.basepath)?);

        self.count = 0;

        self.handle_old_files()?;

        Ok(())
    }
    fn handle_old_files(&mut self) -> io::Result<()> {
        // Find the youngest suffix that is too old, and then remove all suffixes that are older or
        // equally old:
        let mut youngest_old = None;
        // Start from oldest suffix, stop when we find a suffix that is not too old
        let mut result = Ok(());
        for (i, suffix) in self.suffixes.iter().enumerate().rev() {
            if self.suffix_scheme.too_old(&suffix.suffix, i) {
                result = result.and(std::fs::remove_file(suffix.to_path(&self.basepath)));
                youngest_old = Some((*suffix).clone());
            } else {
                break;
            }
        }
        if let Some(youngest_old) = youngest_old {
            // Removes all the too old
            let _ = self.suffixes.split_off(&youngest_old);
        }

        // Compression
        if let Compression::OnRotate(max_file_n) = self.compression {
            let n = (self.suffixes.len() as i32 - max_file_n as i32).max(0) as usize;
            // The oldest N files should be compressed
            let suffixes_to_compress = self
                .suffixes
                .iter()
                .rev()
                .take(n)
                .filter(|info| !info.compressed)
                .cloned()
                .collect::<Vec<_>>();
            for info in suffixes_to_compress {
                // Do the compression
                let path = info.suffix.to_path(&self.basepath);
                compress(&path)?;

                self.suffixes.replace(SuffixInfo {
                    compressed: true,
                    ..info
                });
            }
        }

        result
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
                    file.write_all(buf)?;
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
                    file.write_all(buf)?;
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
