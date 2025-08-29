use crate::Rotator;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::PathBuf;

/// File-based rotator using numbered suffixes.
///
/// Maintains a rotating window of files using numeric extensions, e.g.,
/// `app.log.0` (oldest) through `app.log.N` (newest), where `N = max_files-1`.
/// On rotation, the oldest is deleted, existing files are shifted down one,
/// and the base file is moved to the newest slot.
pub struct NumberedSuffix {
    pub(crate) base_path: PathBuf,
    pub(crate) max_files: usize,
}

impl NumberedSuffix {
    /// Create a new `NumberedSuffix` for the given base path.
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
}

impl Rotator for NumberedSuffix {
    type Writer = File;

    fn initial(&mut self) -> io::Result<Self::Writer> {
        File::create(&self.base_path)
    }

    fn rotate(&mut self, mut current: Self::Writer) -> io::Result<Self::Writer> {
        // Ensure on-disk state is flushed and file handle is closed before renames
        current.flush()?;
        drop(current);

        // We store oldest at .0 and newest rotated at .(max_files-1)
        // On rotation: drop oldest (.0), shift .i -> .(i-1) for i=1..max_files-1, then move base -> .(max_files-1)
        if self.max_files > 0 {
            let oldest = self.base_path.with_extension("0");
            if oldest.exists() {
                let _ = fs::remove_file(&oldest);
            }
            // Shift upwards in age: .1 -> .0, .2 -> .1, ...
            for i in 1..self.max_files {
                let from = self.base_path.with_extension(format!("{}", i));
                let to = self.base_path.with_extension(format!("{}", i - 1));
                if from.exists() {
                    // Remove target if exists to allow rename
                    if to.exists() {
                        let _ = fs::remove_file(&to);
                    }
                    fs::rename(&from, &to)?;
                }
            }
            // Move current base file to newest slot
            if self.base_path.exists() {
                let newest = self
                    .base_path
                    .with_extension(format!("{}", self.max_files - 1));
                if newest.exists() {
                    let _ = fs::remove_file(&newest);
                }
                fs::rename(&self.base_path, newest)?;
            }
        } else {
            // If max_files == 0, we still move current to .0 for consistency
            if self.base_path.exists() {
                let to0 = self.base_path.with_extension("0");
                if to0.exists() {
                    let _ = fs::remove_file(&to0);
                }
                fs::rename(&self.base_path, to0)?;
            }
        }

        // Create new current file
        File::create(&self.base_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write as _;

    #[test]
    fn rotates_and_shifts_numbered_files() {
        let dir = tempfile::tempdir().unwrap();
        let base = dir.path().join("app.log");
        let mut rot = NumberedSuffix::new(base.clone()).max(3);

        // initial
        let mut w = rot.initial().unwrap();
        write!(w, "a").unwrap();
        let mut w = rot.rotate(w).unwrap();
        // base moved to .2 (newest), nothing else exists
        assert!(base.with_extension("2").exists());

        write!(w, "b").unwrap();
        let mut w = rot.rotate(w).unwrap();
        // shift: .2 -> .1, base -> .2
        assert!(base.with_extension("1").exists());
        assert!(base.with_extension("2").exists());

        write!(w, "c").unwrap();
        let _w = rot.rotate(w).unwrap();
        // shift: .1 -> .0, .2 -> .1, base -> .2
        assert!(base.with_extension("0").exists());
        assert!(base.with_extension("1").exists());
        assert!(base.with_extension("2").exists());
    }
}
