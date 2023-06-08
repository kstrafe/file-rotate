# file-rotate

Rotate files with configurable suffix.

Look to the [docs](https://docs.rs/file-rotate/latest/file_rotate/index.html) for explanatory examples of all features, like:
* Using count or timestamp as suffix
* Age-based deletion of log files
* Optional compression
* Getting a list of log files

Limitations / known issues:
* `file-rotate` assumes that no other process or user moves files around in the logging directory, but we want to find a way to [support this](https://github.com/BourgondAries/file-rotate/issues/17)

Following are some supplementary examples to get started.

## Basic example

```rust
use file_rotate::{FileRotate, ContentLimit, compression::Compression, suffix::AppendCount};
use std::{fs, io::Write, path::PathBuf};

fn main() {
    let mut log = FileRotate::new("logs/log", AppendCount::new(2), ContentLimit::Lines(3), Compression::None, None);

    // Write a bunch of lines
    writeln!(log, "Line 1: Hello World!");
    for idx in 2..=10 {
        writeln!(log, "Line {}", idx);
    }
}
```

```
$ ls logs
log  log.1  log.2

$ cat log.2 log.1 log
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10
```

## Example with timestamp suffixes

```rust
let mut log = FileRotate::new(
    "logs/log",
    AppendTimestamp::default(FileLimit::MaxFiles(3)),
    ContentLimit::Lines(3),
    Compression::None,
    None,
);

// Write a bunch of lines
writeln!(log, "Line 1: Hello World!");
for idx in 2..=10 {
    std::thread::sleep(std::time::Duration::from_millis(200));
    writeln!(log, "Line {}", idx);
}
```

```
$ ls logs
log                  log.20210825T151133.1
log.20210825T151133  log.20210825T151134

$ cat logs/*
Line 10
Line 1: Hello World!
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
```

The timestamp format (including the extra trailing `.N`) works by default so that the lexical ordering of filenames equals the chronological ordering.
So it almost works perfectly with `cat logs/*`, except that `log` is smaller (lexically "older") than all the rest. This can of course be fixed with a more complex script to assemble the logs.


## License

This project is licensed under the [MIT license].

[MIT license]: https://github.com/BourgondAries/file-rotate/blob/master/LICENSE

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in file-rotate by you, shall be licensed as MIT, without any additional
terms or conditions.
