use crate::Rotator;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// File-based rotator using date-based (epoch milliseconds) suffixes.
///
/// On rotation, the base file is renamed to `base.<millis>` where `<millis>` is
/// the number of milliseconds since the Unix epoch. If a collision occurs
/// (multiple rotations in the same millisecond), a `-N` suffix is appended to
/// ensure uniqueness.
///
/// Retention is enforced by keeping at most `max_files` rotated files matching
/// the `base.*` pattern and deleting the oldest ones (based on modification
/// time).
pub struct DatedSuffix {
    pub(crate) base_path: PathBuf,
    pub(crate) max_files: usize,
}

impl DatedSuffix {
    /// Create a new `DatedSuffix` for the given base path.
    pub fn new<B>(base: B) -> Self
    where
        B: Into<PathBuf>,
    {
        Self {
            base_path: base.into(),
            max_files: 5,
        }
    }

    /// Set the maximum number of rotated files to retain.
    pub fn max(mut self, max: usize) -> Self {
        self.max_files = max;
        self
    }

    fn make_suffix() -> String {
        let now = SystemTime::now();
        let millis: u128 = now
            .duration_since(UNIX_EPOCH)
            .unwrap_or_else(|_| Duration::from_secs(0))
            .as_millis();
        millis.to_string()
    }

    fn retention_cleanup(&self) -> io::Result<()> {
        // Keep at most max_files rotated files with pattern base_stem.*
        // Note: rotate uses `with_extension`, replacing the original extension
        // with the suffix, so rotated files are named `base_stem.<suffix>`.
        let base_stem = match self.base_path.file_stem() {
            Some(n) => n.to_string_lossy().into_owned(),
            None => return Ok(()),
        };
        let parent = self
            .base_path
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."));
        let mut matches: Vec<(std::path::PathBuf, std::time::SystemTime)> = Vec::new();
        for entry in fs::read_dir(parent)? {
            let entry = entry?;
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                // Expect format: base_stem + "." + anything
                if name.starts_with(&base_stem)
                    && name.get(base_stem.len()..base_stem.len() + 1) == Some(".")
                {
                    let meta = entry.metadata()?;
                    let modified = meta.modified().unwrap_or(SystemTime::UNIX_EPOCH);
                    matches.push((path, modified));
                }
            }
        }
        // Sort newest first
        matches.sort_by(|a, b| b.1.cmp(&a.1));
        if matches.len() > self.max_files {
            for (path, _) in matches.iter().skip(self.max_files) {
                let _ = fs::remove_file(path);
            }
        }
        Ok(())
    }
}

impl Rotator for DatedSuffix {
    type Writer = File;

    fn initial(&mut self) -> io::Result<Self::Writer> {
        File::create(&self.base_path)
    }

    fn rotate(&mut self, mut current: Self::Writer) -> io::Result<Self::Writer> {
        // Ensure file is flushed and handle dropped before rename
        current.flush()?;
        drop(current);

        // Determine unique suffix
        let mut suffix = Self::make_suffix();
        let mut candidate = self.base_path.with_extension(&suffix);
        let mut idx = 1u32;
        while candidate.exists() {
            suffix = format!("{}-{}", suffix, idx);
            candidate = self.base_path.with_extension(&suffix);
            idx += 1;
        }

        // Move base to dated suffix if it exists
        if self.base_path.exists() {
            fs::rename(&self.base_path, &candidate)?;
        }

        // Enforce retention
        let _ = self.retention_cleanup();

        // Create a new base file
        File::create(&self.base_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;

    #[test]
    fn rotates_to_dated_suffix_and_enforces_retention() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path().join("app.log");
        let mut rot = DatedSuffix::new(base.clone()).max(2);

        let mut w = rot.initial().unwrap();
        write!(w, "first").unwrap();
        let mut w = rot.rotate(w).unwrap();
        // After first rotate, there should be at least one file with base.*
        let entries: Vec<_> = std::fs::read_dir(dir.path())
            .unwrap()
            .filter_map(|e| e.ok())
            .collect();
        assert!(entries.iter().any(|e| e.path() != base));

        write!(w, "second").unwrap();
        let mut w = rot.rotate(w).unwrap();
        write!(w, "third").unwrap();
        let _w = rot.rotate(w).unwrap();

        // Retention max 2: ensure we have at most 2 rotated files
        let rotated: Vec<_> = std::fs::read_dir(dir.path())
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p != &base)
            .collect();
        assert!(rotated.len() <= 2);
    }
}
