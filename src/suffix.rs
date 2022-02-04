//! Suffix schemes determine the suffix of rotated files
//!
//! This behaviour is fully extensible through the [SuffixScheme] trait, and two behaviours are
//! provided: [AppendCount] and [AppendTimestamp]
//!
use crate::SuffixInfo;
#[cfg(feature = "chrono04")]
use chrono::{offset::Local, Duration, NaiveDateTime};
use std::{
    cmp::Ordering,
    collections::BTreeSet,
    io,
    path::{Path, PathBuf},
};

/// Representation of a suffix
/// `Ord + PartialOrd`: sort by age of the suffix. Most recent first (smallest).
pub trait Representation: Ord + ToString + Eq + Clone + std::fmt::Debug {
    /// Create path
    fn to_path(&self, basepath: &Path) -> PathBuf {
        PathBuf::from(format!("{}.{}", basepath.display(), self.to_string()))
    }
}

/// How to move files: How to rename, when to delete.
pub trait SuffixScheme {
    /// The representation of suffixes that this suffix scheme uses.
    /// E.g. if the suffix is a number, you can use `usize`.
    type Repr: Representation;

    /// `file-rotate` calls this function when the file at `suffix` needs to be rotated, and moves the log file
    /// accordingly. Thus, this function should not move any files itself.
    ///
    /// If `suffix` is `None`, it means it's the main log file (with path equal to just `basepath`)
    /// that is being rotated.
    ///
    /// Returns the target suffix that the log file should be moved to.
    /// If the target suffix already exists, `rotate_file` is called again with `suffix` set to the
    /// target suffix.  Thus it cascades files by default, and if this is not desired, it's up to
    /// `rotate_file` to return a suffix that does not already exist on disk.
    ///
    /// `newest_suffix` is provided just in case it's useful (depending on the particular suffix scheme, it's not always useful)
    fn rotate_file(
        &mut self,
        basepath: &Path,
        newest_suffix: Option<&Self::Repr>,
        suffix: &Option<Self::Repr>,
    ) -> io::Result<Self::Repr>;

    /// Parse suffix from string.
    fn parse(&self, suffix: &str) -> Option<Self::Repr>;

    /// Whether either the suffix or the chronological file number indicates that the file is old
    /// and should be deleted, depending of course on the file limit.
    /// `file_number` starts at 0 for the most recent suffix.
    fn too_old(&self, suffix: &Self::Repr, file_number: usize) -> bool;

    /// Find all files in the basepath.parent() directory that has path equal to basepath + a valid
    /// suffix. Return sorted collection - sorted from most recent to oldest based on the
    /// [Ord](std::cmp::Ord) implementation of `Self::Repr`.
    fn scan_suffixes(&self, basepath: &Path) -> BTreeSet<SuffixInfo<Self::Repr>> {
        let mut suffixes = BTreeSet::new();
        let filename_prefix = basepath
            .file_name()
            .expect("basepath.file_name()")
            .to_string_lossy();

        // We need the parent directory of the given basepath, but this should also work when the path
        // only has one segment. Thus we prepend the current working dir if the path is relative:
        let basepath = if basepath.is_relative() {
            let mut path = std::env::current_dir().unwrap();
            path.push(basepath);
            path
        } else {
            basepath.to_path_buf()
        };

        let parent = basepath.parent().unwrap();

        let filenames = std::fs::read_dir(parent)
            .unwrap()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().is_file())
            .map(|entry| entry.file_name());
        for filename in filenames {
            let filename = filename.to_string_lossy();
            if !filename.starts_with(&*filename_prefix) {
                continue;
            }
            let (filename, compressed) = prepare_filename(&*filename);
            let suffix_str = filename.strip_prefix(&format!("{}.", filename_prefix));
            if let Some(suffix) = suffix_str.and_then(|s| self.parse(s)) {
                suffixes.insert(SuffixInfo { suffix, compressed });
            }
        }
        suffixes
    }
}
fn prepare_filename(path: &str) -> (&str, bool) {
    path.strip_suffix(".gz")
        .map(|x| (x, true))
        .unwrap_or((path, false))
}

/// Append a number when rotating the file.
/// The greater the number, the older. The oldest files are deleted.
pub struct AppendCount {
    max_files: usize,
}

impl AppendCount {
    /// New suffix scheme, deleting files when the number of rotated files (i.e. excluding the main
    /// file) exceeds `max_files`.
    /// For example, if `max_files` is 3, then the files `log`, `log.1`, `log.2`, `log.3` may exist
    /// but not `log.4`. In other words, `max_files` determines the largest possible suffix number.
    pub fn new(max_files: usize) -> Self {
        Self { max_files }
    }
}

impl Representation for usize {}
impl SuffixScheme for AppendCount {
    type Repr = usize;
    fn rotate_file(
        &mut self,
        _basepath: &Path,
        _: Option<&usize>,
        suffix: &Option<usize>,
    ) -> io::Result<usize> {
        Ok(match suffix {
            Some(suffix) => suffix + 1,
            None => 1,
        })
    }
    fn parse(&self, suffix: &str) -> Option<usize> {
        suffix.parse::<usize>().ok()
    }
    fn too_old(&self, _suffix: &usize, file_number: usize) -> bool {
        file_number >= self.max_files
    }
}

/// Append current timestamp as suffix when rotating files.
/// If the timestamp already exists, an additional number is appended.
///
/// Current limitations:
///  - Neither `format` nor the base filename can include the character `"."`.
///  - The `format` should ensure that the lexical and chronological orderings are the same
#[cfg(feature = "chrono04")]
pub struct AppendTimestamp {
    /// The format of the timestamp suffix
    pub format: &'static str,
    /// The file limit, e.g. when to delete an old file - by age (given by suffix) or by number of files
    pub file_limit: FileLimit,
}

#[cfg(feature = "chrono04")]
impl AppendTimestamp {
    /// With format `"%Y%m%dT%H%M%S"`
    pub fn default(file_limit: FileLimit) -> Self {
        Self {
            format: "%Y%m%dT%H%M%S",
            file_limit,
        }
    }
    /// Create new AppendTimestamp suffix scheme
    pub fn with_format(format: &'static str, file_limit: FileLimit) -> Self {
        Self { format, file_limit }
    }
}

/// Structured representation of the suffixes of AppendTimestamp.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimestampSuffix {
    /// The timestamp
    pub timestamp: String,
    /// Optional number suffix if two timestamp suffixes are the same
    pub number: Option<usize>,
}
impl Representation for TimestampSuffix {}
impl Ord for TimestampSuffix {
    fn cmp(&self, other: &Self) -> Ordering {
        // Most recent = smallest (opposite as the timestamp Ord)
        // Smallest = most recent. Thus, biggest timestamp first. And then biggest number
        match other.timestamp.cmp(&self.timestamp) {
            Ordering::Equal => other.number.cmp(&self.number),
            unequal => unequal,
        }
    }
}
impl PartialOrd for TimestampSuffix {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl std::fmt::Display for TimestampSuffix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.number {
            Some(n) => write!(f, "{}.{}", self.timestamp, n),
            None => write!(f, "{}", self.timestamp),
        }
    }
}

#[cfg(feature = "chrono04")]
impl SuffixScheme for AppendTimestamp {
    type Repr = TimestampSuffix;

    fn rotate_file(
        &mut self,
        _basepath: &Path,
        newest_suffix: Option<&TimestampSuffix>,
        suffix: &Option<TimestampSuffix>,
    ) -> io::Result<TimestampSuffix> {
        assert!(suffix.is_none());
        if suffix.is_none() {
            let now = Local::now().format(self.format).to_string();

            let number = if let Some(newest_suffix) = newest_suffix {
                if newest_suffix.timestamp == now {
                    Some(newest_suffix.number.unwrap_or(0) + 1)
                } else {
                    None
                }
            } else {
                None
            };
            Ok(TimestampSuffix {
                timestamp: now,
                number,
            })
        } else {
            // This rotation scheme dictates that only the main log file should ever be renamed.
            // In debug build the above assert will catch this.
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Critical error in file-rotate algorithm",
            ))
        }
    }
    fn parse(&self, suffix: &str) -> Option<Self::Repr> {
        let (timestamp_str, n) = if let Some(dot) = suffix.find('.') {
            if let Ok(n) = suffix[(dot + 1)..].parse::<usize>() {
                (&suffix[..dot], Some(n))
            } else {
                return None;
            }
        } else {
            (suffix, None)
        };
        NaiveDateTime::parse_from_str(timestamp_str, self.format)
            .map(|_| TimestampSuffix {
                timestamp: timestamp_str.to_string(),
                number: n,
            })
            .ok()
    }
    fn too_old(&self, suffix: &TimestampSuffix, file_number: usize) -> bool {
        match self.file_limit {
            FileLimit::MaxFiles(max_files) => file_number >= max_files,
            FileLimit::Age(age) => {
                let old_timestamp = (Local::now() - age).format(self.format).to_string();
                suffix.timestamp < old_timestamp
            }
        }
    }
}

/// How to determine whether a file should be deleted, in the case of [AppendTimestamp].
#[cfg(feature = "chrono04")]
pub enum FileLimit {
    /// Delete the oldest files if number of files is too high
    MaxFiles(usize),
    /// Delete files whose age exceeds the `Duration` - age is determined by the suffix of the file
    Age(Duration),
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    #[test]
    fn timestamp_ordering() {
        assert!(
            TimestampSuffix {
                timestamp: "2021".to_string(),
                number: None
            } < TimestampSuffix {
                timestamp: "2020".to_string(),
                number: None
            }
        );
        assert!(
            TimestampSuffix {
                timestamp: "2021".to_string(),
                number: Some(1)
            } < TimestampSuffix {
                timestamp: "2021".to_string(),
                number: None
            }
        );
    }

    #[test]
    fn scan_suffixes() {
        let working_dir = tempdir::TempDir::new("file-rotate").unwrap();
        let working_dir = working_dir.path().join("dir");
        let suffix_scheme = AppendTimestamp::default(FileLimit::Age(Duration::weeks(1)));

        // Test `scan_suffixes` for different possible paths given to it
        // (it used to have a bug taking e.g. "log".parent() --> panic)
        for relative_path in ["logs/log", "./log", "log", "../log", "../logs/log"] {
            std::fs::create_dir_all(&working_dir).unwrap();
            println!("Testing relative path: {}", relative_path);
            let relative_path = Path::new(relative_path);

            let log_file = working_dir.join(relative_path);
            let log_dir = log_file.parent().unwrap();
            // Ensure all directories needed exist
            std::fs::create_dir_all(log_dir).unwrap();

            // We cd into working_dir
            std::env::set_current_dir(&working_dir).unwrap();

            // Need to create the log file in order to canonicalize it and then get the parent
            File::create(working_dir.join(&relative_path)).unwrap();
            let canonicalized = relative_path.canonicalize().unwrap();
            let relative_dir = canonicalized.parent().unwrap();

            File::create(relative_dir.join("log.20210911T121830")).unwrap();
            File::create(relative_dir.join("log.20210911T121831.gz")).unwrap();

            let paths = suffix_scheme.scan_suffixes(relative_path);
            assert_eq!(paths.len(), 2);

            // Cleanup
            std::fs::remove_dir_all(&working_dir).unwrap();
        }
    }
}
