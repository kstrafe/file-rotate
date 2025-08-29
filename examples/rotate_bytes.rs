use file_rotate::{rotators::NumberedSuffix, triggers::Bytes, FileRotate};
use std::io::Write;

fn main() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;
    let base = dir.path().join("app.log");

    let rotator = NumberedSuffix::new(base.clone()).max(3);
    let trigger = Bytes::new().limit(10);
    let mut log = FileRotate::new(rotator, trigger)?;

    write!(log, "abcdefghijklmnopqrstuvwxyz0123456789")?;
    log.flush()?;

    // Print where data ended up
    println!(
        ".0: {}",
        std::fs::read_to_string(base.with_extension("0")).unwrap_or_default()
    );
    println!(
        ".1: {}",
        std::fs::read_to_string(base.with_extension("1")).unwrap_or_default()
    );
    println!(
        ".2: {}",
        std::fs::read_to_string(base.with_extension("2")).unwrap_or_default()
    );
    println!(
        "base: {}",
        std::fs::read_to_string(&base).unwrap_or_default()
    );

    Ok(())
}
