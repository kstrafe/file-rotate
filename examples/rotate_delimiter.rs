use file_rotate::{rotators::NumberedSuffix, triggers::Delimiter, FileRotate};
use std::io::Write;

fn main() -> std::io::Result<()> {
    let dir = tempfile::tempdir()?;
    let base = dir.path().join("events");

    let rotator = NumberedSuffix::new(base.clone()).max(2);
    let trigger = Delimiter::new("END\n", true);
    let mut log = FileRotate::new(rotator, trigger)?;

    write!(log, "part1 END\npart2 END\n")?;
    log.flush()?;

    println!(
        ".0: {}",
        std::fs::read_to_string(base.with_extension("0")).unwrap_or_default()
    );
    println!(
        ".1: {}",
        std::fs::read_to_string(base.with_extension("1")).unwrap_or_default()
    );
    println!(
        "base: {}",
        std::fs::read_to_string(&base).unwrap_or_default()
    );

    Ok(())
}
