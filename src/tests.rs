use super::{suffix::*, *};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use tempfile::TempDir;

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
#[cfg(feature = "time")]
fn timestamp_max_files_rotation() {
    let tmp_dir = TempDir::new().unwrap();
    let log_path = tmp_dir.path().join("log");

    let mut log = FileRotate::new(
        &log_path,
        AppendTimestamp::default(FileLimit::MaxFiles(4)),
        ContentLimit::Lines(2),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
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
    let tmp_dir = TempDir::new().unwrap();
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
        AppendTimestamp::default(FileLimit::Age(chrono::Duration::weeks(1))),
        ContentLimit::Lines(1),
        Compression::None,
        #[cfg(unix)]
        None,
    );
    writeln!(log, "trigger\nat\nleast\none\nrotation").unwrap();

    let mut filenames = fs::read_dir(dir)
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
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(4),
        ContentLimit::Lines(2),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
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
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(4),
        ContentLimit::Lines(1),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
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
    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let mut log = FileRotate::new(
        &log_path,
        AppendCount::new(100),
        ContentLimit::BytesSurpassed(1),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
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
#[cfg(feature = "compression")]
fn compression_on_rotation() {
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(3),
        ContentLimit::Lines(1),
        Compression::OnRotate(1), // Keep one file uncompressed
        #[cfg(unix)] None,
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
        let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());

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
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let file_rotate = || {
        FileRotate::new(
            &*log_path.to_string_lossy(),
            AppendCount::new(3),
            ContentLimit::Lines(10000),
            #[cfg(feature = "compression")] Compression::None,
            #[cfg(unix)] None,
        )
    };
    writeln!(file_rotate(), "A").unwrap();
    list(parent);
    writeln!(file_rotate(), "B").unwrap();
    list(parent);

    assert_eq!("A\nB\n", fs::read_to_string(&log_path).unwrap());
}

#[test]
fn byte_count_recalculation() {
    // If there is already some content in the logging file, FileRotate should set its `count`
    // field to the size of the file, so that it rotates at the right time
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");

    fs::write(&log_path, b"a").unwrap();

    let mut file_rotate = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(3),
        ContentLimit::Bytes(2),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    write!(file_rotate, "bc").unwrap();
    assert_eq!(file_rotate.log_paths().len(), 1);
    // The size of the rotated file should be 2 ('ab)
    let rotated_content = fs::read(&file_rotate.log_paths()[0]).unwrap();
    assert_eq!(rotated_content, b"ab");
    // The size of the main file should be 1 ('c')
    let main_content = fs::read(log_path).unwrap();
    assert_eq!(main_content, b"c");
}

#[test]
fn line_count_recalculation() {
    // If there is already some content in the logging file, FileRotate should set its `count`
    // field to the line count of the file, so that it rotates at the right time
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");

    fs::write(&log_path, b"a\n").unwrap();

    let mut file_rotate = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(3),
        ContentLimit::Lines(2),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    // A single line existed before the new logger ('a')
    assert_eq!(file_rotate.count, 1);

    writeln!(file_rotate, "b").unwrap();
    writeln!(file_rotate, "c").unwrap();

    assert_eq!(file_rotate.log_paths().len(), 1);

    // The line count of the rotated file should be 2 ('a' & 'b')
    let mut lines = BufReader::new(File::open(&file_rotate.log_paths()[0]).unwrap()).lines();
    assert_eq!(lines.next().unwrap().unwrap(), "a".to_string());
    assert_eq!(lines.next().unwrap().unwrap(), "b".to_string());

    // The line count of the main file should be 1 ('c')
    let mut lines = BufReader::new(File::open(&log_path).unwrap()).lines();
    assert_eq!(lines.next().unwrap().unwrap(), "c".to_string());
}

#[cfg(unix)]
#[test]
fn unix_file_permissions() {
    let permissions = &[0o600, 0o644];

    for permission in permissions {
        let tmp_dir = TempDir::new().unwrap();
        let parent = tmp_dir.path();
        let log_path = parent.join("log");

        let mut file_rotate = FileRotate::new(
            &*log_path.to_string_lossy(),
            AppendCount::new(3),
            ContentLimit::Lines(2),
            Compression::None,
            Some(*permission),
        );

        // Trigger a rotation by writing three lines
        writeln!(file_rotate, "a").unwrap();
        writeln!(file_rotate, "b").unwrap();
        writeln!(file_rotate, "c").unwrap();

        assert_eq!(file_rotate.log_paths().len(), 1);

        // The file created at initialization time should have the right permissions ...
        let metadata = fs::metadata(&log_path).unwrap();
        assert_eq!(metadata.permissions().mode() & 0o777, *permission);

        // ... and also the one generated through a rotation
        let metadata = fs::metadata(&file_rotate.log_paths()[0]).unwrap();
        assert_eq!(metadata.permissions().mode() & 0o777, *permission);
    }
}

#[test]
fn manual_rotation() {
    // Check that manual rotation works as intented
    let tmp_dir = TempDir::new().unwrap();
    let parent = tmp_dir.path();
    let log_path = parent.join("log");
    let mut log = FileRotate::new(
        &*log_path.to_string_lossy(),
        AppendCount::new(3),
        ContentLimit::None,
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );
    writeln!(log, "A").unwrap();
    log.rotate().unwrap();
    list(parent);
    writeln!(log, "B").unwrap();
    list(parent);

    dbg!(log.log_paths());
    let logs = log.log_paths();
    assert_eq!(logs.len(), 1);
    assert_eq!("A\n", fs::read_to_string(&logs[0]).unwrap());
    assert_eq!("B\n", fs::read_to_string(&log_path).unwrap());
}

#[quickcheck_macros::quickcheck]
fn arbitrary_lines(count: usize) {
    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let count = count.max(1);
    let mut log = FileRotate::new(
        &log_path,
        AppendCount::new(100),
        ContentLimit::Lines(count),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
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
    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    let count = count.max(1);
    let mut log = FileRotate::new(
        &log_path,
        AppendCount::new(100),
        ContentLimit::Bytes(count),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    for _ in 0..count {
        write!(log, "0").unwrap();
    }

    log.flush().unwrap();
    assert!(log.log_paths().is_empty());
    write!(log, "1").unwrap();
    assert!(&log.log_paths()[0].exists());
}

#[test]
#[cfg(feature = "time")]
fn rotate_by_time_frequency() {
    // Test time frequency by hours.
    test_time_frequency(
        "2022-05-03T06:00:12",
        "2022-05-03T06:59:00",
        "2022-05-03T07:01:00",
        "2022-05-03_06-01-00",
        TimeFrequency::Hourly,
        DateFrom::DateHourAgo,
    );

    // Test time frequency by days.
    test_time_frequency(
        "2022-05-02T12:59:59",
        "2022-05-02T23:01:15",
        "2022-05-03T01:01:00",
        "2022-05-02_01-01-00",
        TimeFrequency::Daily,
        DateFrom::DateYesterday,
    );

    // Test time frequency by weeks.
    test_time_frequency(
        "2022-05-02T12:34:02",
        "2022-05-06T11:30:00",
        "2022-05-09T13:01:00",
        "2022-05-08_13-01-00",
        TimeFrequency::Weekly,
        DateFrom::DateYesterday,
    );

    // Test time frequency by months.
    test_time_frequency(
        "2022-03-01T11:50:01",
        "2022-03-30T15:30:10",
        "2022-04-02T05:03:50",
        "2022-04-02_05-03-50",
        TimeFrequency::Monthly,
        DateFrom::Now,
    );

    // Test time frequency by year.
    test_time_frequency(
        "2021-08-31T12:34:02",
        "2021-12-15T15:20:00",
        "2022-09-02T13:01:00",
        "2022-09-01_13-01-00",
        TimeFrequency::Yearly,
        DateFrom::DateYesterday,
    );
}

#[test]
#[cfg(feature = "time")]
fn test_file_limit() {
    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("file");
    let old_file = dir.join("file.2022-02-01");

    File::create(&old_file).unwrap();

    let first = get_fake_date_time("2022-02-02T01:00:00");
    let second = get_fake_date_time("2022-02-03T01:00:00");
    let third = get_fake_date_time("2022-02-04T01:00:00");

    let mut log = FileRotate::new(
        log_path,
        AppendTimestamp::with_format("%Y-%m-%d", FileLimit::MaxFiles(1), DateFrom::DateYesterday),
        ContentLimit::Time(TimeFrequency::Daily),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    mock_time::set_mock_time(first);
    writeln!(log, "1").unwrap();
    mock_time::set_mock_time(second);
    writeln!(log, "2").unwrap();
    mock_time::set_mock_time(third);
    writeln!(log, "3").unwrap();

    assert_eq!(log.log_paths(), [dir.join("file.2022-02-03")]);
    assert!(!old_file.is_file());
}

#[test]
fn test_panic() {
    use std::io::Write;

    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("file");
    // write 9 bytes of data
    {
        let mut log = FileRotate::new(
            &log_path,
            AppendCount::new(2),
            ContentLimit::None,
            #[cfg(feature = "compression")] Compression::None,
            #[cfg(unix)] None,
        );

        write!(log, "nineteen characters").unwrap();
    }

    // set content limit to less than the existing file size
    let mut log = FileRotate::new(
        &log_path,
        AppendCount::new(2),
        ContentLimit::Bytes(8),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    write!(log, "0123").unwrap();

    let log_paths = log.log_paths();
    assert_eq!(
        "nineteen characters",
        fs::read_to_string(&log_paths[0]).unwrap()
    );
    assert_eq!("0123", fs::read_to_string(&log_path).unwrap());
}

#[cfg(feature = "time")]
fn get_fake_date_time(date_time: &str) -> DateTime<Local> {
    let date_obj = NaiveDateTime::parse_from_str(date_time, "%Y-%m-%dT%H:%M:%S");

    Local.from_local_datetime(&date_obj.unwrap()).unwrap()
}

#[cfg(feature = "time")]
fn test_time_frequency(
    old_time: &str,
    second_old_time: &str,
    new_time: &str,
    test_suffix: &str,
    frequency: TimeFrequency,
    date_from: DateFrom,
) {
    let old_time = get_fake_date_time(old_time);
    let new_time = get_fake_date_time(new_time);
    let second_old_time = get_fake_date_time(second_old_time);
    let tmp_dir = TempDir::new().unwrap();
    let dir = tmp_dir.path();
    let log_path = dir.join("log");

    mock_time::set_mock_time(old_time);

    let mut log = FileRotate::new(
        &log_path,
        AppendTimestamp::with_format("%Y-%m-%d_%H-%M-%S", FileLimit::MaxFiles(7), date_from),
        ContentLimit::Time(frequency),
        #[cfg(feature = "compression")] Compression::None,
        #[cfg(unix)] None,
    );

    writeln!(log, "a").unwrap();
    log.flush().unwrap();

    filetime::set_file_mtime(
        log_path,
        filetime::FileTime::from_system_time(old_time.into()),
    )
    .unwrap();

    mock_time::set_mock_time(second_old_time);

    writeln!(log, "b").unwrap();

    mock_time::set_mock_time(new_time);

    writeln!(log, "c").unwrap();

    assert!(&log.log_paths()[0].exists());
    assert_eq!(
        log.log_paths()[0]
            .display()
            .to_string()
            .split('.')
            .collect::<Vec<&str>>()
            .last(),
        Some(&test_suffix)
    );
}
