//! Compression - configuration and implementation
#[cfg(feature = "compression")]
use flate2::write::GzEncoder;
#[cfg(feature = "compression")]
use std::{
    fs::{self, File, OpenOptions},
    path::PathBuf,
};
use std::{io, path::Path};

/// Compression mode - when to compress files.
#[derive(Debug, Clone)]
pub enum Compression {
    /// No compression
    None,
    /// Look for files to compress when rotating.
    /// First argument: How many files to keep uncompressed (excluding the original file).
    /// If `compression` feature is not enabled, this is a no-op.
    OnRotate(usize),
}

/// Compress file at path. If `compression` feature is not enabled, this is a no-op.
pub(crate) fn compress(path: &Path) -> io::Result<()> {
    #[cfg(feature = "compression")]
    {
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
    }

    Ok(())
}
