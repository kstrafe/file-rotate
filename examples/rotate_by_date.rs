use file_rotate::{
    compression::Compression,
    suffix::{AppendTimestamp, DateFrom, FileLimit},
    ContentLimit, FileRotate, TimeFrequency,
};
use std::io::Write;

fn main() {
    let mut log = FileRotate::new(
        "logs/log",
        AppendTimestamp::with_format("%Y-%m-%d", FileLimit::MaxFiles(7), DateFrom::DateYesterday),
        ContentLimit::Time(TimeFrequency::Daily),
        Compression::None,
        None,
    );

    // Write a bunch of lines
    writeln!(log, "Line 1: Hello World!").expect("write log");
    for idx in 2..=10 {
        std::thread::sleep(std::time::Duration::from_millis(500));
        writeln!(log, "Line {}", idx).expect("write log");
    }
}
