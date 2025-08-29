use file_rotate::{rotators::DatedSuffix, triggers::Bytes, FileRotate};
use std::fs;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;
    let base = dir.path().join("app.log");

    let rotator = DatedSuffix::new(base.clone()).max(3);
    let trigger = Bytes::new().limit(8);
    let mut log = FileRotate::new(rotator, trigger)?;

    write!(log, "abcdefgh")?; // rotate at 8 bytes
    write!(log, "ijklmnop")?; // rotate again
    write!(log, "rest")?; // stays in base file

    // List rotated files
    let mut entries: Vec<_> = fs::read_dir(dir.path())?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .collect();
    entries.sort();

    println!("Base file: {}", base.display());
    for path in entries {
        if path != base {
            println!("Rotated: {}", path.display());
        }
    }

    Ok(())
}
