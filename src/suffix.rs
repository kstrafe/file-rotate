#[cfg(feature = "chrono04")]
use chrono::{offset::Local, DateTime, Duration};
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
};

/// How to move files: How to rename, when to delete.
pub trait SuffixScheme {
    /// Returns new suffix to which to move current log file (does not do the move).
    /// Deletes old log files.
    /// Might also do other operations, like moving files in a cascading way.
    fn rotate(&mut self, basepath: &Path) -> String;

    /// Get paths of rotated log files, in order from newest to oldest.
    /// Excludes the suffix-less log file.
    fn log_paths(&mut self, basepath: &Path) -> Vec<PathBuf>;
}

/// Rotated log files get a number as suffix. The greater the number, the older. The oldest files
/// are deleted.
pub struct CountSuffix {
    max_files: usize,
}

impl CountSuffix {
    /// New CountSuffix
    pub fn new(max_files: usize) -> Self {
        Self { max_files }
    }
}

impl SuffixScheme for CountSuffix {
    fn rotate(&mut self, basepath: &Path) -> String {
        /// Make sure that path(count) does not exist, by moving it to path(count+1).
        fn cascade(basepath: &Path, count: usize, max_files: usize) {
            let src = PathBuf::from(format!("{}.{}", basepath.display(), count));
            if src.exists() {
                let dest = PathBuf::from(format!("{}.{}", basepath.display(), count + 1));
                if dest.exists() {
                    cascade(basepath, count + 1, max_files);
                }
                if count >= max_files {
                    // If the file is too old (too big count), delete it,
                    //   (also if count == max_files, because then the .(max_files-1) file will be moved
                    //   to .max_files)
                    let _ = std::fs::remove_file(&src).unwrap();
                } else {
                    // otherwise, rename it.
                    let _ = std::fs::rename(src, dest);
                }
            }
        }
        cascade(basepath, 1, self.max_files);
        "1".to_string()
    }
    fn log_paths(&mut self, basepath: &Path) -> Vec<PathBuf> {
        let filename_prefix = &*basepath
            .file_name()
            .expect("basepath.file_name()")
            .to_string_lossy();
        let filenames = std::fs::read_dir(basepath.parent().expect("basepath.parent()"))
            .unwrap()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().is_file())
            .map(|entry| entry.file_name());
        let mut numbers = Vec::new();
        for filename in filenames {
            let filename = filename.to_string_lossy();
            if !filename.starts_with(&filename_prefix) {
                continue;
            }
            if let Some(dot) = filename.find('.') {
                let suffix = &filename[(dot + 1)..];
                if let Ok(n) = suffix.parse::<usize>() {
                    numbers.push(n);
                } else {
                    continue;
                }
            } else {
                // We don't consider the current (suffix-less) log file.
            }
        }
        // Sort descending - the largest numbers are the oldest and thus should come first
        numbers.sort_by(|x, y| y.cmp(x));
        numbers
            .iter()
            .map(|n| PathBuf::from(format!("{}.{}", basepath.display(), n)))
            .collect::<Vec<_>>()
    }
}

/// Current limitations:
///  - Neither `format` or the base filename can include the character `"."`.
///  - The `format` should ensure that the lexical and chronological orderings are the same
#[cfg(feature = "chrono04")]
pub struct TimestampSuffix {
    /// None means that we don't know the files, and a scan is necessary.
    pub(crate) suffixes: Option<VecDeque<(String, Option<usize>)>>,
    format: &'static str,
    file_limit: FileLimit,
}

#[cfg(feature = "chrono04")]
impl TimestampSuffix {
    /// With format `"%Y%m%dT%H%M%S"`
    pub fn default(file_limit: FileLimit) -> Self {
        Self {
            suffixes: None,
            format: "%Y%m%dT%H%M%S",
            file_limit,
        }
    }
    /// Create new TimestampSuffix suffix scheme
    pub fn with_format(format: &'static str, file_limit: FileLimit) -> Self {
        Self {
            suffixes: None,
            format,
            file_limit,
        }
    }
    /// NOTE: For future use in RotationMode::Custom
    pub fn should_rotate(&self, age: Duration) -> impl Fn(&str) -> bool {
        let format = self.format.to_string();
        move |suffix| {
            let old_timestamp = (Local::now() - age).format(&format).to_string();
            suffix < old_timestamp.as_str()
        }
    }
    pub(crate) fn suffix_to_string(&self, suffix: &(String, Option<usize>)) -> String {
        match suffix.1 {
            Some(n) => format!("{}.{}", Local::now().format(self.format), n),
            None => Local::now().format(self.format).to_string(),
        }
    }
    pub(crate) fn suffix_to_path(
        &self,
        basepath: &Path,
        suffix: &(String, Option<usize>),
    ) -> PathBuf {
        PathBuf::from(format!(
            "{}.{}",
            basepath.display(),
            self.suffix_to_string(suffix)
        ))
    }
    /// Scan files in the log directory to construct the list of files
    fn ensure_suffix_list(&mut self, basepath: &Path) {
        if self.suffixes.is_none() {
            let mut suffixes = VecDeque::new();
            let filename_prefix = &*basepath
                .file_name()
                .expect("basepath.file_name()")
                .to_string_lossy();
            let parent = basepath.parent().unwrap();
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
                if let Some(first_dot) = filename.find('.') {
                    let suffix = &filename[(first_dot + 1)..];
                    let (timestamp_str, n) = if let Some(second_dot) = suffix.find('.') {
                        if let Ok(n) = suffix[(second_dot + 1)..].parse::<usize>() {
                            (&suffix[..second_dot], Some(n))
                        } else {
                            continue;
                        }
                    } else {
                        (suffix, None)
                    };
                    if DateTime::parse_from_str(timestamp_str, self.format).is_ok() {
                        suffixes.push_back((timestamp_str.to_string(), n))
                    }
                } else {
                    // We don't consider the current (suffix-less) log file.
                }
            }
            // Sort in Ascending order (higher value (most recent) first)
            suffixes
                .make_contiguous()
                .sort_by_key(|suffix| self.suffix_to_string(suffix));
            self.suffixes = Some(suffixes);
        }
    }
}
#[cfg(feature = "chrono04")]
impl SuffixScheme for TimestampSuffix {
    fn rotate(&mut self, basepath: &Path) -> String {
        let now = Local::now().format(self.format).to_string();

        self.ensure_suffix_list(basepath);

        // For all existing suffixes that equals `now`, take the max `n`, and add one
        let n = self
            .suffixes
            .as_ref()
            .unwrap()
            .iter()
            .filter(|suffix| suffix.0 == now)
            .map(|suffix| suffix.1.unwrap_or(0))
            .max()
            .map(|n| n + 1);

        // Register the selected suffix as taken
        self.suffixes.as_mut().unwrap().push_back((now.clone(), n));

        // Remove old files
        // Note that the oldest are the first in the list
        let to_delete = match self.file_limit {
            FileLimit::MaxFiles(max_files) => {
                let n_files = self.suffixes.as_ref().unwrap().len();
                if n_files > max_files {
                    n_files - max_files
                } else {
                    0
                }
            }
            FileLimit::Age(age) => {
                let mut to_delete = 0;
                for suffix in self.suffixes.as_ref().unwrap().iter() {
                    let old_timestamp = (Local::now() - age).format(self.format).to_string();
                    let delete = suffix.0 < old_timestamp;
                    if delete {
                        let _ = std::fs::remove_file(self.suffix_to_path(basepath, suffix));
                        to_delete += 1;
                    } else {
                        // Remember that `suffixes` has the oldest entries in the front, we can `break`
                        // once we find an entry that doesn't have to deleted
                        break;
                    }
                }
                to_delete
            }
        };

        // Delete respective entries
        for _ in 0..to_delete {
            let x = self.suffixes.as_mut().unwrap().pop_front();
            println!("DELETE FILE {:?}", x);
        }

        self.suffix_to_string(&(now, n))
    }
    fn log_paths(&mut self, basepath: &Path) -> Vec<PathBuf> {
        self.ensure_suffix_list(basepath);
        self.suffixes
            .as_ref()
            .unwrap()
            .iter()
            .map(|suffix| {
                PathBuf::from(format!(
                    "{}.{}",
                    basepath.display(),
                    self.suffix_to_string(suffix)
                ))
            })
            .collect::<Vec<_>>()
    }
}

/// How to determine if a file should be deleted, in the case of TimestampSuffix.
#[cfg(feature = "chrono04")]
pub enum FileLimit {
    /// Delete the oldest files if number of files is too high
    MaxFiles(usize),
    /// Delete files that have too old timestamp
    Age(Duration),
}
