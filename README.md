# file-rotate

Rotate files with timestamp postfix.

## Rotating by Lines #
  
We can rotate log files by using the amount of lines as a limit.
  
```rust
use file_rotate::{FileRotate, RotationMode};
use std::{fs, io::Write};
  
// Create a new log writer. The first argument is anything resembling a path. The
// basename is used for naming the log files.
//
// Here we choose to limit logs by 10 lines, and have at most 2 rotated log files. This
// makes the total amount of log files 4, since the original file is present as well as
// file 0.
let mut log = FileRotate::new("target/my-log-directory-lines/my-log-file", RotationMode::Lines(3), 2);

// Write a bunch of lines
writeln!(log, "Line 1: Hello World!");
for idx in 2..11 {
  writeln!(log, "Line {}", idx);
}

```

The above code will write fillowing files:

* target/my-log-directory-lines/my-log-file
* target/my-log-directory-lines/my-log-file.20200308125206
* target/my-log-directory-lines/my-log-file.20200308130003

## Rotating by Bytes #
  
Another method of rotation is by bytes instead of lines.
  
```rust
use file_rotate::{FileRotate, RotationMode};
use std::{fs, io::Write};
  
fs::create_dir("target/my-log-directory-bytes");
  
let mut log = FileRotate::new("target/my-log-directory-bytes/my-log-file", RotationMode::Bytes(5), 2);
  
writeln!(log, "Test file");
```
The above code will write fillowing files:

* target/my-log-directory-lines/my-log-file
* target/my-log-directory-lines/my-log-file.20200308125206
* target/my-log-directory-lines/my-log-file.20200308130003
