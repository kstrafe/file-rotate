use super::{compression::*, suffix::*, *};
use tempdir::TempDir;

// Just useful to debug why test doesn't succeed
#[allow(dead_code)]
fn list(dir: &Path) {
    let files = fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| (entry.file_name(), fs::read_to_string(entry.path())))
        .collect::<Vec<_>>();
    println!("Files on disk:");
    for (name, content) in files {
        println!("{:?}: {:?}", name, content);
    }
}

#[test]
fn timestamp_max_files_rotation() {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let log_path = tmp_dir.path().join("log");

    let mut log = FileRotate::new(
        &log_path,
        TimestampSuffixScheme::default(FileLimit::MaxFiles(4)),
        ContentLimit::Lines(2),
        Compression::None,
    );

    // Write 9 lines
    // This should result in 5 files in total (4 rotated files). The main file will have one line.
    write!(log, "a\nb\nc\nd\ne\nf\ng\nh\ni\n").unwrap();
    let log_paths = log.log_paths();
    assert_eq!(log_paths.len(), 4);

    // Log names should be sorted. Low (old timestamp) to high (more recent timestamp)
    let mut log_paths_sorted = log_paths.clone();
    log_paths_sorted.sort();
    assert_eq!(log_paths, log_paths_sorted);

    assert_eq!("a\nb\n", fs::read_to_string(&log_paths[0]).unwrap());
    assert_eq!("c\nd\n", fs::read_to_string(&log_paths[1]).unwrap());
    assert_eq!("e\nf\n", fs::read_to_string(&log_paths[2]).unwrap());
    assert_eq!("g\nh\n", fs::read_to_string(&log_paths[3]).unwrap());
    assert_eq!("i\n", fs::read_to_string(&log_path).unwrap());

    // Write 4 more lines
    write!(log, "j\nk\nl\nm\n").unwrap();
    let log_paths = log.log_paths();
    assert_eq!(log_paths.len(), 4);
    let mut log_paths_sorted = log_paths.clone();
    log_paths_sorted.sort();
    assert_eq!(log_paths, log_paths_sorted);

    list(tmp_dir.path());
    assert_eq!("e\nf\n", fs::read_to_string(&log_paths[0]).unwrap());
    assert_eq!("g\nh\n", fs::read_to_string(&log_paths[1]).unwrap());
    assert_eq!("i\nj\n", fs::read_to_string(&log_paths[2]).unwrap());
    assert_eq!("k\nl\n", fs::read_to_string(&log_paths[3]).unwrap());
    assert_eq!("m\n", fs::read_to_string(&log_path).unwrap());
}
#[test]
#[cfg(feature = "chrono04")]
fn timestamp_max_age_deletion() {
    // In order not to have to sleep, and keep it deterministic, let's already create the log files and see how FileRotate
    // cleans up the old ones.
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    // One recent file:
    let recent_file = chrono::offset::Local::now()
        .format("log.%Y%m%dT%H%M%S")
        .to_string();
    File::create(dir.join(&recent_file)).unwrap();
    // Two very old files:
    File::create(dir.join("log.20200825T151133")).unwrap();
    File::create(dir.join("log.20200825T151133.1")).unwrap();

    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        TimestampSuffixScheme::default(FileLimit::Age(chrono::Duration::weeks(1))),
        ContentLimit::Lines(1),
        Compression::None,
    );
    writeln!(log, "trigger\nat\nleast\none\nrotation").unwrap();

    let mut filenames = std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    filenames.sort();
    assert!(filenames.contains(&"log".to_string()));
    assert!(filenames.contains(&recent_file));
    assert!(!filenames.contains(&"log.20200825T151133".to_string()));
    assert!(!filenames.contains(&"log.20200825T151133.1".to_string()));
}
#[test]
fn count_max_files_rotation() {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        CountSuffix::new(4),
        ContentLimit::Lines(2),
        Compression::None,
    );

    // Write 9 lines
    // This should result in 5 files in total (4 rotated files). The main file will have one line.
    write!(log, "a\nb\nc\nd\ne\nf\ng\nh\ni\n").unwrap(); // 9 lines
    let log_paths = vec![
        parent.join("log.4"),
        parent.join("log.3"),
        parent.join("log.2"),
        parent.join("log.1"),
    ];
    assert_eq!(log_paths, log.log_paths());
    assert_eq!("a\nb\n", fs::read_to_string(&log_paths[0]).unwrap());
    assert_eq!("c\nd\n", fs::read_to_string(&log_paths[1]).unwrap());
    assert_eq!("e\nf\n", fs::read_to_string(&log_paths[2]).unwrap());
    assert_eq!("g\nh\n", fs::read_to_string(&log_paths[3]).unwrap());
    assert_eq!("i\n", fs::read_to_string(&log_path).unwrap());

    // Write 4 more lines
    write!(log, "j\nk\nl\nm\n").unwrap();
    list(parent);
    assert_eq!(log_paths, log.log_paths());

    assert_eq!("e\nf\n", fs::read_to_string(&log_paths[0]).unwrap());
    assert_eq!("g\nh\n", fs::read_to_string(&log_paths[1]).unwrap());
    assert_eq!("i\nj\n", fs::read_to_string(&log_paths[2]).unwrap());
    assert_eq!("k\nl\n", fs::read_to_string(&log_paths[3]).unwrap());
    assert_eq!("m\n", fs::read_to_string(&log_path).unwrap());
}

#[test]
fn rotate_to_deleted_directory() {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        CountSuffix::new(4),
        ContentLimit::Lines(1),
        Compression::None,
    );

    write!(log, "a\nb\n").unwrap();
    assert_eq!("", fs::read_to_string(&log_path).unwrap());
    assert_eq!("a\n", fs::read_to_string(&log.log_paths()[0]).unwrap());

    let _ = fs::remove_dir_all(parent);

    // Will fail to write `"c"`
    writeln!(log, "c").unwrap();
    log.flush().unwrap();

    // But the next `write` will succeed
    writeln!(log, "d").unwrap();
    assert_eq!("", fs::read_to_string(&log_path).unwrap());
    assert_eq!("d\n", fs::read_to_string(&log.log_paths()[1]).unwrap());
}

#[test]
fn write_complete_record_until_bytes_surpassed() {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let mut log = FileRotate::new(
        &log_path,
        TimestampSuffixScheme::default(FileLimit::MaxFiles(100)),
        ContentLimit::BytesSurpassed(1),
        Compression::None,
    );

    write!(log, "0123456789").unwrap();
    log.flush().unwrap();
    assert!(log_path.exists());
    // shouldn't exist yet - because entire record was written in one shot
    assert!(log.log_paths().is_empty());

    // This should create the second file
    write!(log, "0123456789").unwrap();
    log.flush().unwrap();
    assert!(&log.log_paths()[0].exists());
}

#[test]
fn compression_on_rotation() {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        CountSuffix::new(3),
        ContentLimit::Lines(1),
        Compression::OnRotate(1), // Keep one file uncompressed
    );

    writeln!(log, "A").unwrap();
    writeln!(log, "B").unwrap();
    writeln!(log, "C").unwrap();
    list(tmp_dir.path());

    let log_paths = log.log_paths();

    assert_eq!(
        log_paths,
        vec![
            parent.join("log.3.gz"),
            parent.join("log.2.gz"),
            parent.join("log.1"),
        ]
    );

    assert_eq!("", fs::read_to_string(&log_path).unwrap());

    fn compress(text: &str) -> Vec<u8> {
        let mut encoder =
            flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());

        encoder.write_all(text.as_bytes()).unwrap();
        encoder.finish().unwrap()
    }
    assert_eq!(compress("A\n"), fs::read(&log.log_paths()[0]).unwrap());
    assert_eq!(compress("B\n"), fs::read(&log.log_paths()[1]).unwrap());
    assert_eq!("C\n", fs::read_to_string(&log.log_paths()[2]).unwrap());
}

#[test]
fn no_truncate() {
    // Don't truncate log file if it already exists
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let file_rotate = || {
        FileRotate::new(
            &*log_path.to_string_lossy(),
            CountSuffix::new(3),
            ContentLimit::Lines(10000),
            Compression::None,
        )
    };
    writeln!(file_rotate(), "A").unwrap();
    list(parent);
    writeln!(file_rotate(), "B").unwrap();
    list(parent);

    assert_eq!("A\nB\n", fs::read_to_string(&log_path).unwrap());
}

#[quickcheck_macros::quickcheck]
fn arbitrary_lines(count: usize) {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let count = count.max(1);
    let mut log = FileRotate::new(
        &log_path,
        TimestampSuffixScheme::default(FileLimit::MaxFiles(100)),
        ContentLimit::Lines(count),
        Compression::None,
    );

    for _ in 0..count - 1 {
        writeln!(log).unwrap();
    }

    log.flush().unwrap();
    assert!(log.log_paths().is_empty());
    writeln!(log).unwrap();
    assert!(Path::new(&log.log_paths()[0]).exists());
}

#[quickcheck_macros::quickcheck]
fn arbitrary_bytes(count: usize) {
    let tmp_dir = TempDir::new("file-rotate-test").unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let count = count.max(1);
    let mut log = FileRotate::new(
        &log_path,
        TimestampSuffixScheme::default(FileLimit::MaxFiles(100)),
        ContentLimit::Bytes(count),
        Compression::None,
    );

    for _ in 0..count {
        write!(log, "0").unwrap();
    }

    log.flush().unwrap();
    assert!(log.log_paths().is_empty());
    write!(log, "1").unwrap();
    assert!(&log.log_paths()[0].exists());
}
