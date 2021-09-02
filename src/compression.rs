use flate2::write::GzEncoder;
use std::{
    fs::{self, File, OpenOptions},
    io,
    path::{Path, PathBuf},
};

/// In the future, maybe stream compression
#[derive(Debug, Clone)]
pub enum Compression {
    /// No compression
    None,
    /// Look for files to compress when rotating.
    /// First argument: How many files to keep uncompressed (excluding the original file)
    OnRotate(usize),
}

pub(crate) fn compress(path: &Path) -> io::Result<()> {
    let dest_path = PathBuf::from(format!("{}.gz", path.display()));

    let mut src_file = File::open(path)?;
    let dest_file = OpenOptions::new()
        .write(true)
        .create(true)
        .append(false)
        .open(&dest_path)?;

    assert!(path.exists());
    assert!(dest_path.exists());
    let mut encoder = GzEncoder::new(dest_file, flate2::Compression::default());
    io::copy(&mut src_file, &mut encoder)?;

    fs::remove_file(path)?;

    Ok(())
}
